module Web.GPU.GPUBufferBindingType
  ( GPUBufferBindingType
  , readOnlyStorage
  , storage
  , uniform
  ) where

import Prelude

newtype GPUBufferBindingType = GPUBufferBindingType String

derive instance Eq GPUBufferBindingType
derive instance Ord GPUBufferBindingType
derive newtype instance Show GPUBufferBindingType

uniform :: GPUBufferBindingType
uniform = GPUBufferBindingType "uniform"

storage :: GPUBufferBindingType
storage = GPUBufferBindingType "storage"

readOnlyStorage :: GPUBufferBindingType
readOnlyStorage = GPUBufferBindingType "read-only-storage"