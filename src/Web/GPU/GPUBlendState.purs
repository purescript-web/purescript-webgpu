module Web.GPU.GPUBlendState where

import Data.Newtype (class Newtype)
import Web.GPU.GPUBlendComponent (GPUBlendComponent)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUBlendState = GPUBlendState
  ( RequiredAndOptional (color :: GPUBlendComponent, alpha :: GPUBlendComponent)
      ()
  )

derive instance Newtype GPUBlendState _