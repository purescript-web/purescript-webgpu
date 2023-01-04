module Web.GPU.GPUBindGroupEntry
  ( BufferBinding
  , GPUBufferBinding
  , add
  , bindGroupEntryForBufferBinding
  , bindGroupEntryForExternalTexture
  , bindGroupEntryForSampler
  , bindGroupEntryForTextureView
  , empty
  , gpuBufferBinding
  )
  where

import Data.Reflectable (class Reflectable, reflectType)
import Prim.Int (class ToString, class Compare)
import Prim.Ordering (LT)
import Prim.Row (class Cons, class Lacks)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.ConvertibleOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Web.GPU.Internal.Types (GPUBindGroupEntries, GPUBindGroupEntry, GPUBuffer, GPUExternalTexture, GPUSampler, GPUTextureView)
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

bindGroupEntryForBufferBinding :: forall (i :: Int). Reflectable i Int => Proxy i -> GPUBufferBinding -> GPUBindGroupEntry i GPUBufferBinding
bindGroupEntryForBufferBinding binding resource = unsafeCoerce { binding: reflectType binding, resource }

bindGroupEntryForSampler :: forall (i :: Int). Reflectable i Int => Proxy i -> GPUSampler -> GPUBindGroupEntry i GPUSampler
bindGroupEntryForSampler binding resource = unsafeCoerce { binding: reflectType binding, resource }

bindGroupEntryForTextureView :: forall (i :: Int). Reflectable i Int => Proxy i -> GPUTextureView -> GPUBindGroupEntry i GPUTextureView
bindGroupEntryForTextureView binding resource = unsafeCoerce { binding: reflectType binding, resource }

bindGroupEntryForExternalTexture :: forall (i :: Int). Reflectable i Int => Proxy i -> GPUExternalTexture -> GPUBindGroupEntry i GPUExternalTexture
bindGroupEntryForExternalTexture binding resource = unsafeCoerce { binding: reflectType binding, resource }

empty :: GPUBindGroupEntries ()
empty = unsafeCoerce []

foreign import unsafeAdd :: forall x y z. x -> y -> z

add
  :: forall i key binding prev current
   . Compare (-1) i LT
  => ToString i key
  => Lacks key prev
  => Cons key binding prev current
  => GPUBindGroupEntry i binding
  -> GPUBindGroupEntries prev
  -> GPUBindGroupEntries current
add = unsafeAdd

infixr 5 add as :+.