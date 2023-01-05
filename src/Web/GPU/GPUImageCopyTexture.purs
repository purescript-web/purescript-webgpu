module Web.GPU.GPUImageCopyTexture where

import Data.Newtype (class Newtype)
import Web.GPU.GPUOrigin3D (GPUOrigin3D)
import Web.GPU.GPUTexture (GPUTexture)
import Web.GPU.GPUTextureAspect (GPUTextureAspect)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUIntegerCoordinate)

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