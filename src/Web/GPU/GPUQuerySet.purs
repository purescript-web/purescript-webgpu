-- @inline export destroyImpl arity=1
-- @inline export destroy arity=1
-- @inline export typeImpl arity=1
-- @inline export type' arity=1
-- @inline export countImpl arity=1
-- @inline export count arity=1
module Web.GPU.GPUQuerySet
  ( GPUQuerySet
  , count
  , destroy
  , type'
  ) where

import Prelude

import Effect (Effect)
import Web.GPU.GPUQueryType (GPUQueryType)
import Web.GPU.Internal.Types (GPUSize32)

data GPUQuerySet

foreign import destroyImpl :: GPUQuerySet -> Effect Unit

destroy :: GPUQuerySet -> Effect Unit
destroy = destroyImpl

foreign import typeImpl :: GPUQuerySet -> Effect GPUQueryType

type' :: GPUQuerySet -> Effect GPUQueryType
type' = typeImpl

foreign import countImpl :: GPUQuerySet -> Effect GPUSize32

count :: GPUQuerySet -> Effect Int
count = countImpl