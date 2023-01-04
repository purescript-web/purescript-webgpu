module Web.GPU.Internal.Types
  ( Binding
  , GPU
  , GPUAdapter
  , GPUBindGroup
  , GPUBindGroupDescriptor
  , GPUBindGroupEntries(..)
  , GPUBindGroupEntry
  , GPUBindGroupLayout
  , GPUBindGroupLayoutDescriptor
  , GPUBindGroupLayoutEntry
  , GPUBuffer
  , GPUBufferBindingLayout
  , GPUCommandEncoder
  , GPUComputePipeline
  , GPUDevice
  , GPUExternalTexture
  , GPUExternalTextureBindingLayout
  , GPUPipelineLayout
  , GPUPipelineLayoutDescriptor(..)
  , GPUQuerySet
  , GPUQueue
  , GPURenderPipeline
  , GPUSampler
  , GPUSamplerBindingLayout
  , GPUShaderModule
  , GPUShaderModuleCompilationHint
  , GPUStorageTextureBindingLayout
  , GPUTexture
  , GPUTextureBindingLayout
  , GPUTextureView
  ) where

data GPU
data GPUAdapter
data GPUDevice
data GPUQueue
data GPUBuffer
data GPUTexture
data GPUSampler
data GPUExternalTexture
data GPUBindGroupLayout (entries :: Row Binding)
data GPUPipelineLayout (bindingLayouts :: Row Type)
data GPUBindGroupLayoutDescriptor (entries :: Row Binding)
data GPUPipelineLayoutDescriptor (entries :: Row Type)
data Binding
data GPUBindGroupLayoutEntry (i :: Int) (binding :: Binding)

foreign import data GPUBufferBindingLayout :: Binding
foreign import data GPUSamplerBindingLayout :: Binding
foreign import data GPUTextureBindingLayout :: Binding
foreign import data GPUStorageTextureBindingLayout :: Binding
foreign import data GPUExternalTextureBindingLayout :: Binding
data GPUBindGroupEntry (i :: Int) (resource :: Type)
data GPUBindGroupEntries (entries :: Row Type)
data GPUTextureView
data GPUBindGroupDescriptor (layoutEntries :: Row Binding) (entries :: Row Type)
data GPUBindGroup (layoutEntries :: Row Binding) (entries :: Row Type)
data GPUShaderModuleCompilationHint
data GPUShaderModule
data GPUComputePipeline
data GPURenderPipeline
data GPUCommandEncoder
data GPUQuerySet