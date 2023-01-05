module Web.GPU.GPUBindGroupEntry where

import Data.Newtype (class Newtype)
import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.GPUBuffer (GPUBuffer)
import Web.GPU.GPUExternalTexture (GPUExternalTexture)
import Web.GPU.GPUSampler (GPUSampler)
import Web.GPU.GPUTextureView (GPUTextureView)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUIndex32, GPUSize64)

data GPUBindGroupEntry

class AsGPUBindGroupEntry e where
  gpuBindGroupEntry :: GPUIndex32 -> e -> GPUBindGroupEntry

instance AsGPUBindGroupEntry GPUSampler where
  gpuBindGroupEntry binding resource = unsafeCoerce
    { binding, resource: unsafeCoerce resource }

instance AsGPUBindGroupEntry GPUTextureView where
  gpuBindGroupEntry binding resource = unsafeCoerce
    { binding, resource: unsafeCoerce resource }

newtype GPUBufferBinding = GPUBufferBinding
  ( RequiredAndOptional (buffer :: GPUBuffer)
      (offset :: GPUSize64, size :: GPUSize64)
  )

derive instance Newtype GPUBufferBinding _

instance AsGPUBindGroupEntry GPUBufferBinding where
  gpuBindGroupEntry binding resource = unsafeCoerce
    { binding, resource: unsafeCoerce resource }

instance AsGPUBindGroupEntry GPUExternalTexture where
  gpuBindGroupEntry binding resource = unsafeCoerce
    { binding, resource: unsafeCoerce resource }