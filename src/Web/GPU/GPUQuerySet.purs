-- @inline export destroy arity=1
-- @inline export type' arity=1
-- @inline export count arity=1
module Web.GPU.GPUQuerySet
  ( GPUQuerySet
  , count
  , destroy
  , type'
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Web.GPU.GPUQueryType (GPUQueryType)
import Web.GPU.Internal.Types (GPUSize32)

data GPUQuerySet

foreign import destroyImpl :: EffectFn1 GPUQuerySet  Unit
destroy :: GPUQuerySet -> Effect Unit
destroy a = runEffectFn1 destroyImpl a

foreign import typeImpl :: EffectFn1 GPUQuerySet  GPUQueryType
type' :: GPUQuerySet -> Effect GPUQueryType
type' a = runEffectFn1 typeImpl a

foreign import countImpl :: EffectFn1 GPUQuerySet  GPUSize32
count :: GPUQuerySet -> Effect Int
count a = runEffectFn1 countImpl a