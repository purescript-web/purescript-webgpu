module Web.GPU.GPUComputePassDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUComputePassTimestampWrite (GPUComputePassTimestampWrite)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUComputePassDescriptor = GPUComputePassDescriptor
  ( RequiredAndOptional ()
      (timestampWrites :: Array GPUComputePassTimestampWrite, label :: String)
  )

derive instance Newtype GPUComputePassDescriptor _