module Web.GPU.GPUExternalTextureBindingLayout where

import Data.Newtype (class Newtype)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUExternalTextureBindingLayout = GPUExternalTextureBindingLayout
  (RequiredAndOptional () ())

derive instance Newtype GPUExternalTextureBindingLayout _