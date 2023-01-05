module Web.GPU.GPUQuerySet
  ( count
  , destroy
  , type'
  ) where

import Prelude

import Effect (Effect)
import Web.GPU.GPUQueryType (GPUQueryType)
import Web.GPU.Internal.Types (GPUQuerySet, GPUSize32)

foreign import destroyImpl :: GPUQuerySet -> Effect Unit

destroy :: GPUQuerySet -> Effect Unit
destroy = destroyImpl

foreign import typeImpl :: GPUQuerySet -> Effect GPUQueryType

type' :: GPUQuerySet -> Effect GPUQueryType
type' = typeImpl

foreign import countImpl :: GPUQuerySet -> Effect GPUSize32

count :: GPUQuerySet -> Effect Int
count = countImpl