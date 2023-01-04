module Web.GPU.GPUColorTargetState where

import Data.Newtype (class Newtype)
import Web.GPU.GPUBlendState (GPUBlendState)
import Web.GPU.GPUColorWrite (GPUColorWriteFlags)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUColorTargetState = GPUColorTargetState
  ( RequiredAndOptional
      ( format :: GPUTextureFormat
      )
      ( blend :: GPUBlendState
      , writeMask :: GPUColorWriteFlags
      )
  )

derive instance Newtype GPUColorTargetState _