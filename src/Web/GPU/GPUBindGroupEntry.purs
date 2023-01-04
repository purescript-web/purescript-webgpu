module Web.GPU.GPUBindGroupEntry
  ( BufferBinding
  , GPUBufferBinding
  , bindGroupEntryForBufferBinding
  , bindGroupEntryForExternalTexture
  , bindGroupEntryForSampler
  , bindGroupEntryForTextureView
  , gpuBufferBinding
  ) where

import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.ConvertibleOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Web.GPU.Internal.Types (GPUBindGroupEntry, GPUBuffer, GPUExternalTexture, GPUSampler, GPUTextureView)
import Web.GPU.Internal.Undefinable (Undefinable, defined, undefined)
import Web.GPU.Internal.Unsigned (GPUSize64)

newtype GPUBufferBinding = GPUBufferBinding
  { buffer :: GPUBuffer
  , offset :: Undefinable GPUSize64
  , size :: Undefinable GPUSize64
  }

type GPUBufferBindingOptional =
  ( offset :: Undefinable GPUSize64
  , size :: Undefinable GPUSize64
  )

type GPUBufferBindingAll =
  (buffer :: GPUBuffer | GPUBufferBindingOptional)

defaultGPUBufferBinding :: { | GPUBufferBindingOptional }
defaultGPUBufferBinding =
  { offset: undefined
  , size: undefined
  }

data BufferBinding = BufferBinding

instance ConvertOption BufferBinding "offset" GPUSize64 (Undefinable GPUSize64) where
  convertOption _ _ = defined

instance ConvertOption BufferBinding "size" GPUSize64 (Undefinable GPUSize64) where
  convertOption _ _ = defined

gpuBufferBinding
  :: forall provided
   . ConvertOptionsWithDefaults BufferBinding { | GPUBufferBindingOptional } { | provided } { | GPUBufferBindingAll }
  => { | provided }
  -> GPUBufferBinding
gpuBufferBinding provided = GPUBufferBinding all
  where
  all :: { | GPUBufferBindingAll }
  all = convertOptionsWithDefaults BufferBinding defaultGPUBufferBinding provided

bindGroupEntryForBufferBinding :: Int -> GPUBufferBinding -> GPUBindGroupEntry
bindGroupEntryForBufferBinding binding resource = unsafeCoerce { binding, resource }

bindGroupEntryForSampler :: Int -> GPUSampler -> GPUBindGroupEntry
bindGroupEntryForSampler binding resource = unsafeCoerce { binding, resource }

bindGroupEntryForTextureView :: Int -> GPUTextureView -> GPUBindGroupEntry
bindGroupEntryForTextureView binding resource = unsafeCoerce { binding, resource }

bindGroupEntryForExternalTexture :: Int -> GPUExternalTexture -> GPUBindGroupEntry
bindGroupEntryForExternalTexture binding resource = unsafeCoerce { binding, resource }
