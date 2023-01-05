module Web.GPU.GPUImageCopyExternalImage where

import Data.Newtype (class Newtype)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUImageCopyExternalImageSource, GPUOrigin2D)

newtype GPUImageCopyExternalImage = GPUImageCopyExternalImage
  ( RequiredAndOptional (source :: GPUImageCopyExternalImageSource)
      ( origin :: GPUOrigin2D
      , flipY :: Boolean
      )
  )

derive instance Newtype GPUImageCopyExternalImage _