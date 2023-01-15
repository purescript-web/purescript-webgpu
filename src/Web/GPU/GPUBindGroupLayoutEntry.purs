module Web.GPU.GPUBindGroupLayoutEntry where

import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.GPUBufferBindingLayout (GPUBufferBindingLayout)
import Web.GPU.GPUExternalTextureBindingLayout (GPUExternalTextureBindingLayout)
import Web.GPU.GPUSamplerBindingLayout (GPUSamplerBindingLayout)
import Web.GPU.GPUShaderStage (GPUShaderStageFlags)
import Web.GPU.GPUStorageTextureBindingLayout (GPUStorageTextureBindingLayout)
import Web.GPU.GPUTextureBindingLayout (GPUTextureBindingLayout)
import Web.GPU.Internal.Types (GPUIndex32)

data GPUBindGroupLayoutEntry

class AsGPUBindGroupLayoutEntry e where
  gpuBindGroupLayoutEntry
    :: GPUIndex32 -> GPUShaderStageFlags -> e -> GPUBindGroupLayoutEntry

instance AsGPUBindGroupLayoutEntry GPUBufferBindingLayout where
  gpuBindGroupLayoutEntry binding visibility buffer = unsafeCoerce
    { binding, visibility, buffer }

instance AsGPUBindGroupLayoutEntry GPUSamplerBindingLayout where
  gpuBindGroupLayoutEntry binding visibility sampler = unsafeCoerce
    { binding, visibility, sampler }

instance AsGPUBindGroupLayoutEntry GPUTextureBindingLayout where
  gpuBindGroupLayoutEntry binding visibility texture = unsafeCoerce
    { binding, visibility, texture }

instance AsGPUBindGroupLayoutEntry GPUStorageTextureBindingLayout where
  gpuBindGroupLayoutEntry binding visibility storageTexture = unsafeCoerce
    { binding, visibility, storageTexture }

instance AsGPUBindGroupLayoutEntry GPUExternalTextureBindingLayout where
  gpuBindGroupLayoutEntry binding visibility externalTexture =
    unsafeCoerce { binding, visibility, externalTexture }