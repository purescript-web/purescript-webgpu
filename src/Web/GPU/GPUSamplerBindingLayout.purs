module Web.GPU.GPUSamplerBindingLayout where

import Data.Newtype (class Newtype)
import Web.GPU.GPUSamplerBindingType (GPUSamplerBindingType)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUSamplerBindingLayout = GPUSamplerBindingLayout
  (RequiredAndOptional () (type :: GPUSamplerBindingType))

derive instance Newtype GPUSamplerBindingLayout _