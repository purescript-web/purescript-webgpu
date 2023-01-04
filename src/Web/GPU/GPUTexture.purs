module Web.GPU.GPUTexture
  ( TextureViewDescriptor
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

import Effect (Effect)
import Web.GPU.GPUTextureAspect (GPUTextureAspect)
import Web.GPU.GPUTextureDimension (GPUTextureDimension)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.GPUTextureUsage (GPUTextureUsage)
import Web.GPU.GPUTextureViewDimension (GPUTextureViewDimension)
import Web.GPU.Internal.ConvertibleOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Web.GPU.Internal.Types (GPUTexture, GPUTextureView)
import Web.GPU.Internal.Undefinable (Undefinable, defined, undefined)
import Web.GPU.Internal.Unsigned (GPUIntegerCoordinate, GPUSize32)

type GPUTextureViewDescriptorOptional =
  ( aspect :: Undefinable GPUTextureAspect
  , baseMipLevel :: Undefinable GPUIntegerCoordinate
  , baseArrayLayer :: Undefinable GPUIntegerCoordinate
  )

type GPUTextureViewDescriptor =
  ( format :: GPUTextureFormat
  , dimension :: GPUTextureViewDimension
  , mipLevelCount :: GPUIntegerCoordinate
  , arrayLayerCount :: GPUIntegerCoordinate
  | GPUTextureViewDescriptorOptional
  )

defaultGPUTextureViewDescriptorOptions :: { | GPUTextureViewDescriptorOptional }
defaultGPUTextureViewDescriptorOptions =
  { aspect: undefined
  , baseMipLevel: undefined
  , baseArrayLayer: undefined
  }

data TextureViewDescriptor = TextureViewDescriptor

instance ConvertOption TextureViewDescriptor "aspect" GPUTextureAspect (Undefinable GPUTextureAspect) where
  convertOption _ _ = defined

instance ConvertOption TextureViewDescriptor "baseMipLevel" GPUIntegerCoordinate (Undefinable GPUIntegerCoordinate) where
  convertOption _ _ = defined

instance ConvertOption TextureViewDescriptor "baseArrayLayer" GPUIntegerCoordinate (Undefinable GPUIntegerCoordinate) where
  convertOption _ _ = defined

foreign import createViewWithDescriptorImpl :: GPUTexture -> { | GPUTextureViewDescriptor } -> Effect GPUTextureView

createViewWithDescriptor
  :: forall provided
   . ConvertOptionsWithDefaults TextureViewDescriptor { | GPUTextureViewDescriptorOptional } { | provided } { | GPUTextureViewDescriptor }
  => GPUTexture
  -> { | provided }
  -> Effect GPUTextureView
createViewWithDescriptor gpuDevice provided = createViewWithDescriptorImpl gpuDevice all
  where
  all :: { | GPUTextureViewDescriptor }
  all = convertOptionsWithDefaults TextureViewDescriptor defaultGPUTextureViewDescriptorOptions provided

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

foreign import depthOrArrayLayersImpl :: GPUTexture -> Effect GPUIntegerCoordinate

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
