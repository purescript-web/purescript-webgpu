module Web.GPU.GPURenderBundleDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPURenderBundleDescriptor = GPURenderBundleDescriptor
  (RequiredAndOptional () (label :: String))

derive instance Newtype GPURenderBundleDescriptor _