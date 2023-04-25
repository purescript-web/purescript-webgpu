-- @inline export createViewWithDescriptor arity=2
-- @inline export createView arity=2
-- @inline export destroy arity=1
-- @inline export width arity=1
-- @inline export height arity=1
-- @inline export depthOrArrayLayers arity=1
-- @inline export mipLevelCount arity=1
-- @inline export sampleCount arity=1
-- @inline export dimension arity=1
-- @inline export format arity=1
-- @inline export usage arity=1
module Web.GPU.GPUTexture
  ( GPUTexture
  , createViewWithDescriptor
  , createView
  , destroy
  , width
  , height
  , depthOrArrayLayers
  , mipLevelCount
  , sampleCount
  , dimension
  , format
  , usage
  ) where

import Prelude
import Effect.Uncurried(EffectFn1, runEffectFn1,EffectFn2, runEffectFn2)

import Effect (Effect)
import Web.GPU.GPUTextureDimension (GPUTextureDimension)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.GPUTextureUsage (GPUTextureUsage)
import Web.GPU.GPUTextureView (GPUTextureView)
import Web.GPU.GPUTextureViewDescriptor (GPUTextureViewDescriptor)
import Web.GPU.Internal.Types (GPUIntegerCoordinate, GPUSize32)

data GPUTexture

foreign import createViewWithDescriptorImpl :: EffectFn2 GPUTexture GPUTextureViewDescriptor  GPUTextureView
createViewWithDescriptor
  :: GPUTexture
  -> GPUTextureViewDescriptor
  -> Effect GPUTextureView
createViewWithDescriptor a b = runEffectFn2 createViewWithDescriptorImpl a b

foreign import createViewImpl :: EffectFn1 GPUTexture  GPUTextureView
createView
  :: GPUTexture
  -> Effect GPUTextureView
createView a = runEffectFn1 createViewImpl a

foreign import destroyImpl :: EffectFn1 GPUTexture  Unit
destroy :: GPUTexture -> Effect Unit
destroy a = runEffectFn1 destroyImpl a

foreign import widthImpl :: EffectFn1 GPUTexture  GPUIntegerCoordinate
width :: GPUTexture -> Effect GPUIntegerCoordinate
width a = runEffectFn1 widthImpl a

foreign import heightImpl :: EffectFn1 GPUTexture  GPUIntegerCoordinate
height :: GPUTexture -> Effect GPUIntegerCoordinate
height a = runEffectFn1 heightImpl a

foreign import depthOrArrayLayersImpl :: EffectFn1 GPUTexture  GPUIntegerCoordinate
depthOrArrayLayers :: GPUTexture -> Effect GPUIntegerCoordinate
depthOrArrayLayers a = runEffectFn1 depthOrArrayLayersImpl a

foreign import mipLevelCountImpl :: EffectFn1 GPUTexture  GPUIntegerCoordinate
mipLevelCount :: GPUTexture -> Effect GPUIntegerCoordinate
mipLevelCount a = runEffectFn1 mipLevelCountImpl a

foreign import sampleCountImpl :: EffectFn1 GPUTexture  GPUSize32
sampleCount :: GPUTexture -> Effect GPUSize32
sampleCount a = runEffectFn1 sampleCountImpl a

foreign import dimensionImpl :: EffectFn1 GPUTexture  GPUTextureDimension
dimension :: GPUTexture -> Effect GPUTextureDimension
dimension a = runEffectFn1 dimensionImpl a

foreign import formatImpl :: EffectFn1 GPUTexture  GPUTextureFormat
format :: GPUTexture -> Effect GPUTextureFormat
format a = runEffectFn1 formatImpl a

foreign import usageImpl :: EffectFn1 GPUTexture  GPUTextureUsage
usage :: GPUTexture -> Effect GPUTextureUsage
usage a = runEffectFn1 usageImpl a