module Web.GPU.GPUQuerySetDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUQueryType (GPUQueryType)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUSize32)

newtype GPUQuerySetDescriptor = GPUQuerySetDescriptor
  ( RequiredAndOptional (type :: GPUQueryType, count :: GPUSize32)
      (label :: String)
  )

derive instance Newtype GPUQuerySetDescriptor _