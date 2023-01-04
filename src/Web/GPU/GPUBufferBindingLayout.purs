module Web.GPU.GPUBufferBindingLayout where

import Data.Newtype (class Newtype)
import Web.GPU.GPUBufferBindingType (GPUBufferBindingType)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUSize64)

newtype GPUBufferBindingLayout = GPUBufferBindingLayout
  ( RequiredAndOptional ()
      ( type :: GPUBufferBindingType
      , hasDynamicOffset :: Boolean
      , minBindingSize :: GPUSize64
      )
  )

derive instance Newtype GPUBufferBindingLayout _