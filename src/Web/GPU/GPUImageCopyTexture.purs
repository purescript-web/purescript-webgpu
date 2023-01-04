module Web.GPU.GPUImageCopyTexture where

import Data.Newtype (class Newtype)
import Web.GPU.GPUTextureAspect (GPUTextureAspect)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUIntegerCoordinate, GPUOrigin3D, GPUTexture)

newtype GPUImageCopyTexture = GPUImageCopyTexture
  ( RequiredAndOptional
      ( texture :: GPUTexture
      )
      ( mipLevel :: GPUIntegerCoordinate
      , origin :: GPUOrigin3D
      , aspect :: GPUTextureAspect
      )
  )

derive instance Newtype GPUImageCopyTexture _