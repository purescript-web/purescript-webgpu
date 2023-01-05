module Web.GPU.GPUTextureViewDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUTextureAspect (GPUTextureAspect)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.GPUTextureViewDimension (GPUTextureViewDimension)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUIntegerCoordinate)

newtype GPUTextureViewDescriptor = GPUTextureViewDescriptor
  ( RequiredAndOptional
      ()
      ( format :: GPUTextureFormat
      , dimension :: GPUTextureViewDimension
      , aspect :: GPUTextureAspect
      , baseMipLevel :: GPUIntegerCoordinate
      , mipLevelCount :: GPUIntegerCoordinate
      , baseArrayLayer :: GPUIntegerCoordinate
      , arrayLayerCount :: GPUIntegerCoordinate
      , label :: String
      )
  )

derive instance Newtype GPUTextureViewDescriptor _