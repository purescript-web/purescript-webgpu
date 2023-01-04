module Web.GPU.GPUTexture
  ( createViewWithDescriptor
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

import Effect (Effect)
import Web.GPU.GPUTextureDimension (GPUTextureDimension)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.GPUTextureUsage (GPUTextureUsage)
import Web.GPU.GPUTextureViewDescriptor (GPUTextureViewDescriptor)
import Web.GPU.Internal.Types (GPUTexture, GPUTextureView, GPUIntegerCoordinate, GPUSize32)

foreign import createViewWithDescriptorImpl
  :: GPUTexture -> GPUTextureViewDescriptor -> Effect GPUTextureView

createViewWithDescriptor
  :: GPUTexture
  -> GPUTextureViewDescriptor
  -> Effect GPUTextureView
createViewWithDescriptor = createViewWithDescriptorImpl

foreign import createViewImpl :: GPUTexture -> Effect GPUTextureView

createView
  :: GPUTexture
  -> Effect GPUTextureView
createView = createViewImpl

foreign import destroyImpl :: GPUTexture -> Effect Unit

destroy :: GPUTexture -> Effect Unit
destroy = destroyImpl

foreign import widthImpl :: GPUTexture -> Effect GPUIntegerCoordinate

width :: GPUTexture -> Effect GPUIntegerCoordinate
width = widthImpl

foreign import heightImpl :: GPUTexture -> Effect GPUIntegerCoordinate

height :: GPUTexture -> Effect GPUIntegerCoordinate
height = heightImpl

foreign import depthOrArrayLayersImpl
  :: GPUTexture -> Effect GPUIntegerCoordinate

depthOrArrayLayers :: GPUTexture -> Effect GPUIntegerCoordinate
depthOrArrayLayers = depthOrArrayLayersImpl

foreign import mipLevelCountImpl :: GPUTexture -> Effect GPUIntegerCoordinate

mipLevelCount :: GPUTexture -> Effect GPUIntegerCoordinate
mipLevelCount = mipLevelCountImpl

foreign import sampleCountImpl :: GPUTexture -> Effect GPUSize32

sampleCount :: GPUTexture -> Effect GPUSize32
sampleCount = sampleCountImpl

foreign import dimensionImpl :: GPUTexture -> Effect GPUTextureDimension

dimension :: GPUTexture -> Effect GPUTextureDimension
dimension = dimensionImpl

foreign import formatImpl :: GPUTexture -> Effect GPUTextureFormat

format :: GPUTexture -> Effect GPUTextureFormat
format = formatImpl

foreign import usageImpl :: GPUTexture -> Effect GPUTextureUsage

usage :: GPUTexture -> Effect GPUTextureUsage
usage = usageImpl
