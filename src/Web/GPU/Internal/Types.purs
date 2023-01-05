module Web.GPU.Internal.Types
  ( BufferSource
  , GPU
  , GPUAdapter
  , GPUBindGroup
  , GPUBindGroupEntry
  , GPUBindGroupLayout
  , GPUBindGroupLayoutEntry
  , GPUBuffer
  , GPUBufferDynamicOffset
  , GPUCanvasContext
  , GPUColor
  , GPUCommandBuffer
  , GPUCommandEncoder
  , GPUComputePassEncoder
  , GPUComputePipeline
  , GPUDepthBias
  , GPUDevice
  , GPUExtent3D
  , GPUExternalTexture
  , GPUImageCopyExternalImageSource
  , GPUIndex32
  , GPUIntegerCoordinate
  , GPUOrigin2D
  , GPUOrigin3D
  , GPUPipelineLayout
  , GPUQuerySet
  , GPUQueue
  , GPURenderBundle
  , GPURenderBundleEncoder
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
  , ImageBitmap
  , Long
  , OffscreenCanvas
  , UnsignedLong
  , UnsignedLongLong
  , UnsignedShort
  ) where

-- todo: add this to the PS ecosystem
data ImageBitmap
-- todo: add this to the PS ecosystem
data OffscreenCanvas
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
data GPURenderBundleEncoder
data GPUComputePassEncoder
data GPUCommandBuffer
data GPURenderBundle
data GPUImageCopyExternalImageSource
data GPUCanvasContext
data GPUOrigin3D
data GPUExtent3D
data GPUOrigin2D
data BufferSource
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