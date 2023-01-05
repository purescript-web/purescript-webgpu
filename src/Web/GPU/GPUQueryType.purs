module Web.GPU.GPUQueryType
  ( GPUQueryType
  , occlusion
  , timestamp
  ) where

import Prelude

newtype GPUQueryType = GPUQueryType String

derive instance Eq GPUQueryType
derive instance Ord GPUQueryType
derive newtype instance Show GPUQueryType

occlusion :: GPUQueryType
occlusion = GPUQueryType "occlusion"

timestamp :: GPUQueryType
timestamp = GPUQueryType "timestamp"