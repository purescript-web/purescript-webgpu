module Web.GPU.GPUSupportedLimits
  ( GPUSupportedLimits
  , GPUSupportedLimits'
  , Identity'
  , UndefinableGPUSupportedLimits
  , defaultGPUSupportedLimits
  ) where

import Web.GPU.Internal.Undefinable (Undefinable, undefined)
import Web.GPU.Internal.Unsigned (UnsignedLong, UnsignedLongLong)

type Identity' (a :: Type) = a

-- limits
type GPUSupportedLimits' (f :: Type -> Type) =
  ( maxTextureDimension1D :: f UnsignedLong
  , maxTextureDimension2D :: f UnsignedLong
  , maxTextureDimension3D :: f UnsignedLong
  , maxTextureArrayLayers :: f UnsignedLong
  , maxBindGroups :: f UnsignedLong
  , maxBindingsPerBindGroup :: f UnsignedLong
  , maxDynamicUniformBuffersPerPipelineLayout :: f UnsignedLong
  , maxDynamicStorageBuffersPerPipelineLayout :: f UnsignedLong
  , maxSampledTexturesPerShaderStage :: f UnsignedLong
  , maxSamplersPerShaderStage :: f UnsignedLong
  , maxStorageBuffersPerShaderStage :: f UnsignedLong
  , maxStorageTexturesPerShaderStage :: f UnsignedLong
  , maxUniformBuffersPerShaderStage :: f UnsignedLong
  , maxUniformBufferBindingSize :: f UnsignedLongLong
  , maxStorageBufferBindingSize :: f UnsignedLongLong
  , minUniformBufferOffsetAlignment :: f UnsignedLong
  , minStorageBufferOffsetAlignment :: f UnsignedLong
  , maxVertexBuffers :: f UnsignedLong
  , maxBufferSize :: f UnsignedLongLong
  , maxVertexAttributes :: f UnsignedLong
  , maxVertexBufferArrayStride :: f UnsignedLong
  , maxInterStageShaderComponents :: f UnsignedLong
  , maxInterStageShaderVariables :: f UnsignedLong
  , maxColorAttachments :: f UnsignedLong
  , maxColorAttachmentBytesPerSample :: f UnsignedLong
  , maxComputeWorkgroupStorageSize :: f UnsignedLong
  , maxComputeInvocationsPerWorkgroup :: f UnsignedLong
  , maxComputeWorkgroupSizeX :: f UnsignedLong
  , maxComputeWorkgroupSizeY :: f UnsignedLong
  , maxComputeWorkgroupSizeZ :: f UnsignedLong
  , maxComputeWorkgroupsPerDimension :: f UnsignedLong
  )

type UndefinableGPUSupportedLimits = GPUSupportedLimits' Undefinable
type GPUSupportedLimits = GPUSupportedLimits' Identity'

defaultGPUSupportedLimits :: { | UndefinableGPUSupportedLimits }
defaultGPUSupportedLimits =
  { maxTextureDimension1D: undefined
  , maxTextureDimension2D: undefined
  , maxTextureDimension3D: undefined
  , maxTextureArrayLayers: undefined
  , maxBindGroups: undefined
  , maxBindingsPerBindGroup: undefined
  , maxDynamicUniformBuffersPerPipelineLayout: undefined
  , maxDynamicStorageBuffersPerPipelineLayout: undefined
  , maxSampledTexturesPerShaderStage: undefined
  , maxSamplersPerShaderStage: undefined
  , maxStorageBuffersPerShaderStage: undefined
  , maxStorageTexturesPerShaderStage: undefined
  , maxUniformBuffersPerShaderStage: undefined
  , maxUniformBufferBindingSize: undefined
  , maxStorageBufferBindingSize: undefined
  , minUniformBufferOffsetAlignment: undefined
  , minStorageBufferOffsetAlignment: undefined
  , maxVertexBuffers: undefined
  , maxBufferSize: undefined
  , maxVertexAttributes: undefined
  , maxVertexBufferArrayStride: undefined
  , maxInterStageShaderComponents: undefined
  , maxInterStageShaderVariables: undefined
  , maxColorAttachments: undefined
  , maxColorAttachmentBytesPerSample: undefined
  , maxComputeWorkgroupStorageSize: undefined
  , maxComputeInvocationsPerWorkgroup: undefined
  , maxComputeWorkgroupSizeX: undefined
  , maxComputeWorkgroupSizeY: undefined
  , maxComputeWorkgroupSizeZ: undefined
  , maxComputeWorkgroupsPerDimension: undefined
  }