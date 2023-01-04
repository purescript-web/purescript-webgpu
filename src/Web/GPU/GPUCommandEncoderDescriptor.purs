module Web.GPU.GPUCommandEncoderDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUCommandEncoderDescriptor = GPUCommandEncoderDescriptor
  (RequiredAndOptional () (label :: String))

derive instance Newtype GPUCommandEncoderDescriptor _