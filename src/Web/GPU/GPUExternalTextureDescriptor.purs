module Web.GPU.GPUExternalTextureDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.PredefinedColorSpace (PredefinedColorSpace)
import Web.HTML (HTMLVideoElement)

newtype GPUExternalTextureDescriptor = GPUExternalTextureDescriptor
  ( RequiredAndOptional (source :: HTMLVideoElement)
      (colorSpace :: PredefinedColorSpace, label :: String)
  )

derive instance Newtype GPUExternalTextureDescriptor _