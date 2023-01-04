module Web.GPU.GPUDeviceDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUFeatureName (GPUFeatureName)
import Web.GPU.GPUSupportedLimits (GPUSupportedLimits)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUQueueDescriptor = GPUQueueDescriptor
  (RequiredAndOptional () (label :: String))

derive instance Newtype GPUQueueDescriptor _

newtype GPURequiredLimits = GPURequiredLimits
  (RequiredAndOptional () GPUSupportedLimits)

derive instance Newtype GPURequiredLimits _
newtype GPUDeviceDescriptor = GPUDeviceDescriptor
  ( RequiredAndOptional ()
      ( requiredFeatures :: Array GPUFeatureName
      , requiredLimits :: GPURequiredLimits
      , defaultQueue :: GPUQueueDescriptor
      , label :: String
      )
  )

derive instance Newtype GPUDeviceDescriptor _