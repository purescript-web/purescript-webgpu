module Web.GPU.GPUTextureDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUExtent3D (GPUExtent3D)
import Web.GPU.GPUTextureDimension (GPUTextureDimension)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.GPUTextureUsage (GPUTextureUsageFlags)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUIntegerCoordinate, GPUSize32)

newtype GPUTextureDescriptor = GPUTextureDescriptor
  ( RequiredAndOptional
      ( size :: GPUExtent3D
      , format :: GPUTextureFormat
      , usage :: GPUTextureUsageFlags
      )
      ( dimension :: GPUTextureDimension
      , sampleCount :: GPUSize32
      , viewFormats :: Array GPUTextureFormat
      , mipLevelCount :: GPUIntegerCoordinate
      , label :: String
      )
  )

derive instance Newtype GPUTextureDescriptor _