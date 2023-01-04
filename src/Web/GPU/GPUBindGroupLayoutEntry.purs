module Web.GPU.GPUBindGroupLayoutEntry where

import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.GPUBufferBindingLayout (GPUBufferBindingLayout)
import Web.GPU.GPUExternalTextureBindingLayout (GPUExternalTextureBindingLayout)
import Web.GPU.GPUSamplerBindingLayout (GPUSamplerBindingLayout)
import Web.GPU.GPUShaderStage (GPUShaderStageFlags)
import Web.GPU.GPUStorageTextureBindingLayout (GPUStorageTextureBindingLayout)
import Web.GPU.GPUTextureBindingLayout (GPUTextureBindingLayout)
import Web.GPU.Internal.Types (GPUBindGroupLayoutEntry, GPUIndex32)

gpuBufferBindingLayout
  :: GPUIndex32
  -> GPUShaderStageFlags
  -> GPUBufferBindingLayout
  -> GPUBindGroupLayoutEntry
gpuBufferBindingLayout binding visibility buffer = unsafeCoerce
  { binding, visibility, buffer }

gpuSamplerBindingLayout
  :: GPUIndex32
  -> GPUShaderStageFlags
  -> GPUSamplerBindingLayout
  -> GPUBindGroupLayoutEntry
gpuSamplerBindingLayout binding visibility sampler = unsafeCoerce
  { binding, visibility, sampler }

gpuTextureBindingLayout
  :: GPUIndex32
  -> GPUShaderStageFlags
  -> GPUTextureBindingLayout
  -> GPUBindGroupLayoutEntry
gpuTextureBindingLayout binding visibility texture = unsafeCoerce
  { binding, visibility, texture }

gpuStorageTextureBindingLayout
  :: GPUIndex32
  -> GPUShaderStageFlags
  -> GPUStorageTextureBindingLayout
  -> GPUBindGroupLayoutEntry
gpuStorageTextureBindingLayout binding visibility storageTexture = unsafeCoerce
  { binding, visibility, storageTexture }

gpuExternalTextureBindingLayout
  :: GPUIndex32
  -> GPUShaderStageFlags
  -> GPUExternalTextureBindingLayout
  -> GPUBindGroupLayoutEntry
gpuExternalTextureBindingLayout binding visibility externalTexture =
  unsafeCoerce { binding, visibility, externalTexture }
