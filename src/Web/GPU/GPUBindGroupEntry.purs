module Web.GPU.GPUBindGroupEntry where

import Data.Newtype (class Newtype)
import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUBindGroupEntry, GPUBuffer, GPUExternalTexture, GPUIndex32, GPUSampler, GPUTextureView, GPUSize64)

class GPUBindGroupEntry e where
  gpuBindGroupEntry :: GPUIndex32 -> e -> GPUBindGroupEntry

instance GPUBindGroupEntry GPUSampler where
  gpuBindGroupEntry binding resource = unsafeCoerce
    { binding, resource: unsafeCoerce resource }

instance GPUBindGroupEntry GPUTextureView where
  gpuBindGroupEntry binding resource = unsafeCoerce
    { binding, resource: unsafeCoerce resource }

newtype GPUBufferBinding = GPUBufferBinding
  ( RequiredAndOptional (buffer :: GPUBuffer)
      (offset :: GPUSize64, size :: GPUSize64)
  )

derive instance Newtype GPUBufferBinding _

instance GPUBindGroupEntry GPUBufferBinding where
  gpuBindGroupEntry binding resource = unsafeCoerce
    { binding, resource: unsafeCoerce resource }

instance GPUBindGroupEntry GPUExternalTexture where
  gpuBindGroupEntry binding resource = unsafeCoerce
    { binding, resource: unsafeCoerce resource }