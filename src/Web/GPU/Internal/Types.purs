module Web.GPU.Internal.Types
  ( GPU
  , GPUAdapter
  , GPUBindGroup
  , GPUBindGroupEntry
  , GPUBindGroupLayout
  , GPUBindGroupLayoutEntry
  , GPUBuffer
  , GPUBufferDynamicOffset
  , GPUColor
  , GPUCommandBuffer(..)
  , GPUCommandEncoder
  , GPUComputePassEncoder
  , GPUComputePipeline
  , GPUDepthBias
  , GPUDevice
  , GPUExtent3D
  , GPUExternalTexture
  , GPUIndex32
  , GPUIntegerCoordinate
  , GPUOrigin3D
  , GPUPipelineLayout
  , GPUQuerySet
  , GPUQueue
  , GPURenderBundle(..)
  , GPURenderPassEncoder
  , GPURenderPipeline
  , GPUSampleMask
  , GPUSampler
  , GPUShaderModule
  , GPUShaderModuleCompilationHint
  , GPUSignedOffset32
  , GPUSize32
  , GPUSize64
  , GPUStencilValue
  , GPUTexture
  , GPUTextureView
  , Long
  , UnsignedLong
  , UnsignedLongLong
  , UnsignedShort
  ) where

data GPU
data GPUAdapter
data GPUDevice
data GPUQueue
data GPUBuffer
data GPUTexture
data GPUSampler
data GPUExternalTexture
data GPUBindGroupLayout
data GPUPipelineLayout
data GPUBindGroupLayoutEntry
data GPUBindGroupEntry
data GPUTextureView
data GPUBindGroup
data GPUShaderModuleCompilationHint
data GPUShaderModule
data GPUComputePipeline
data GPURenderPipeline
data GPUCommandEncoder
data GPUQuerySet
data GPUColor
data GPURenderPassEncoder
data GPUComputePassEncoder
data GPUCommandBuffer
data GPURenderBundle
data GPUOrigin3D
data GPUExtent3D
type Long = Int
type UnsignedShort = Int
type UnsignedLong = Int
type UnsignedLongLong = Int
type GPUSize64 = UnsignedLongLong
type GPUIntegerCoordinate = UnsignedLong
type GPUSize32 = UnsignedLong
type GPUIndex32 = UnsignedLong
type GPUStencilValue = UnsignedLong
type GPUSampleMask = UnsignedLong
type GPUDepthBias = Long
type GPUBufferDynamicOffset = UnsignedLong
type GPUSignedOffset32 = Long