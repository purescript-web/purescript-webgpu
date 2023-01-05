module Web.GPU.GPUSupportedLimits
  ( GPUSupportedLimits
  ) where

import Web.GPU.Internal.Types (UnsignedLong, UnsignedLongLong)

type GPUSupportedLimits =
  ( maxTextureDimension1D :: UnsignedLong
  , maxTextureDimension2D :: UnsignedLong
  , maxTextureDimension3D :: UnsignedLong
  , maxTextureArrayLayers :: UnsignedLong
  , maxBindGroups :: UnsignedLong
  , maxBindingsPerBindGroup :: UnsignedLong
  , maxDynamicUniformBuffersPerPipelineLayout :: UnsignedLong
  , maxDynamicStorageBuffersPerPipelineLayout :: UnsignedLong
  , maxSampledTexturesPerShaderStage :: UnsignedLong
  , maxSamplersPerShaderStage :: UnsignedLong
  , maxStorageBuffersPerShaderStage :: UnsignedLong
  , maxStorageTexturesPerShaderStage :: UnsignedLong
  , maxUniformBuffersPerShaderStage :: UnsignedLong
  , maxUniformBufferBindingSize :: UnsignedLongLong
  , maxStorageBufferBindingSize :: UnsignedLongLong
  , minUniformBufferOffsetAlignment :: UnsignedLong
  , minStorageBufferOffsetAlignment :: UnsignedLong
  , maxVertexBuffers :: UnsignedLong
  , maxBufferSize :: UnsignedLongLong
  , maxVertexAttributes :: UnsignedLong
  , maxVertexBufferArrayStride :: UnsignedLong
  , maxInterStageShaderComponents :: UnsignedLong
  , maxInterStageShaderVariables :: UnsignedLong
  , maxColorAttachments :: UnsignedLong
  , maxColorAttachmentBytesPerSample :: UnsignedLong
  , maxComputeWorkgroupStorageSize :: UnsignedLong
  , maxComputeInvocationsPerWorkgroup :: UnsignedLong
  , maxComputeWorkgroupSizeX :: UnsignedLong
  , maxComputeWorkgroupSizeY :: UnsignedLong
  , maxComputeWorkgroupSizeZ :: UnsignedLong
  , maxComputeWorkgroupsPerDimension :: UnsignedLong
  )