module Web.GPU.GPUMultisampleState where

import Data.Newtype (class Newtype)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUSize32, GPUSampleMask)

newtype GPUMultisampleState = GPUMultisampleState
  ( RequiredAndOptional
      ()
      ( count :: GPUSize32
      , mask :: GPUSampleMask
      , alphaToCoverageEnabled :: Boolean
      )
  )

derive instance Newtype GPUMultisampleState _