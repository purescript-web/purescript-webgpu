module Web.GPU.GPUImageCopyExternalImage where

import Data.Newtype (class Newtype)
import Web.GPU.GPUImageCopyExternalImageSource (GPUImageCopyExternalImageSource)
import Web.GPU.GPUOrigin2D (GPUOrigin2D)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUImageCopyExternalImage = GPUImageCopyExternalImage
  ( RequiredAndOptional (source :: GPUImageCopyExternalImageSource)
      ( origin :: GPUOrigin2D
      , flipY :: Boolean
      )
  )

derive instance Newtype GPUImageCopyExternalImage _