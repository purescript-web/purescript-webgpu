module Web.GPU.GPUBufferDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUBufferUsage (GPUBufferUsageFlags)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUSize64)

newtype GPUBufferDescriptor = GPUBufferDescriptor
  ( RequiredAndOptional (size :: GPUSize64, usage :: GPUBufferUsageFlags)
      (mappedAtCreation :: Boolean, label :: String)
  )

derive instance Newtype GPUBufferDescriptor _