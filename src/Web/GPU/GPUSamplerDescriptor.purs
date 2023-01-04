module Web.GPU.GPUSamplerDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUAddressMode (GPUAddressMode)
import Web.GPU.GPUCompareFunction (GPUCompareFunction)
import Web.GPU.GPUFilterMode (GPUFilterMode)
import Web.GPU.GPUMipmapFilterMode (GPUMipmapFilterMode)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (UnsignedShort)

newtype GPUSamplerDescriptor = GPUSamplerDescriptor
  ( RequiredAndOptional ()
      ( addressModeU :: GPUAddressMode
      , addressModeV :: GPUAddressMode
      , addressModeW :: GPUAddressMode
      , magFilter :: GPUFilterMode
      , minFilter :: GPUFilterMode
      , mipmapFilter :: GPUMipmapFilterMode
      , lodMinClamp :: Number
      , lodMaxClamp :: Number
      , compare :: GPUCompareFunction
      , maxAnisotropy :: UnsignedShort
      , label :: String
      )
  )

derive instance Newtype GPUSamplerDescriptor _