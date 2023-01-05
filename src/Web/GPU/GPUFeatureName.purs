module Web.GPU.GPUFeatureName
  ( GPUFeatureName
  , bgra8unormStorage
  , depth32floatStencil8
  , depthClipControl
  , indirectFirstInstance
  , rg11b10ufloatRenderable
  , shaderF16
  , textureCompressionAstc
  , textureCompressionBc
  , textureCompressionEtc2
  , timestampQuery
  ) where

import Prelude

newtype GPUFeatureName = GPUFeatureName String

derive instance Eq GPUFeatureName
derive instance Ord GPUFeatureName
derive newtype instance Show GPUFeatureName
depthClipControl = GPUFeatureName "depth-clip-control" :: GPUFeatureName
depth32floatStencil8 = GPUFeatureName "depth32float-stencil8" :: GPUFeatureName
textureCompressionBc = GPUFeatureName "texture-compression-bc" :: GPUFeatureName
textureCompressionEtc2 =
  GPUFeatureName "texture-compression-etc2" :: GPUFeatureName

textureCompressionAstc =
  GPUFeatureName "texture-compression-astc" :: GPUFeatureName

timestampQuery = GPUFeatureName "timestamp-query" :: GPUFeatureName
indirectFirstInstance =
  GPUFeatureName "indirect-first-instance" :: GPUFeatureName

shaderF16 = GPUFeatureName "shader-f16" :: GPUFeatureName
bgra8unormStorage = GPUFeatureName "bgra8unorm-storage" :: GPUFeatureName
rg11b10ufloatRenderable =
  GPUFeatureName "rg11b10ufloat-renderable" :: GPUFeatureName