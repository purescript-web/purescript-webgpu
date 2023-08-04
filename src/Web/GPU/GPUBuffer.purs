-- @inline export size arity=1
-- @inline export usage arity=1
-- @inline export mapState arity=1
-- @inline export mapAsync arity=2
-- @inline export mapAsyncWithOffset arity=3
-- @inline export mapAsyncWithSize arity=3
-- @inline export mapAsyncWithOffsetAndSize arity=4
-- @inline export getMappedRange arity=1
-- @inline export getMappedRangeWithOffset arity=2
-- @inline export getMappedRangeWithSize arity=2
-- @inline export getMappedRangeWithOffsetAndSize arity=3
-- @inline export unmap arity=1
-- @inline export destroy arity=1
module Web.GPU.GPUBuffer
  ( GPUBuffer
  , destroy
  , getMappedRange
  , getMappedRangeWithOffset
  , getMappedRangeWithOffsetAndSize
  , getMappedRangeWithSize
  , mapAsync
  , mapAsyncWithOffset
  , mapAsyncWithOffsetAndSize
  , mapAsyncWithSize
  , mapState
  , size
  , unmap
  , usage
  ) where

import Prelude
import Effect.Uncurried (EffectFn1, runEffectFn1, EffectFn2, runEffectFn2, EffectFn3, runEffectFn3, EffectFn4, runEffectFn4)

import Data.ArrayBuffer.Types (ArrayBuffer)
import Effect (Effect)
import Web.GPU.GPUBufferMapState (GPUBufferMapState)
import Web.GPU.GPUBufferUsage (GPUBufferUsage)
import Web.GPU.GPUMapMode (GPUMapMode)
import Web.GPU.Internal.Types (GPUSize64)
import Promise (Promise)

data GPUBuffer

foreign import sizeImpl :: EffectFn1 GPUBuffer GPUSize64

size :: GPUBuffer -> Effect GPUSize64
size a = runEffectFn1 sizeImpl a

foreign import usageImpl :: EffectFn1 GPUBuffer GPUBufferUsage

usage :: GPUBuffer -> Effect GPUBufferUsage
usage a = runEffectFn1 usageImpl a

foreign import mapStateImpl :: EffectFn1 GPUBuffer GPUBufferMapState

mapState :: GPUBuffer -> Effect GPUBufferMapState
mapState a = runEffectFn1 mapStateImpl a

foreign import mapAsyncImpl :: EffectFn2 GPUBuffer GPUMapMode (Promise Unit)

mapAsync :: GPUBuffer -> GPUMapMode -> Effect (Promise Unit)
mapAsync a b = runEffectFn2 mapAsyncImpl a b

foreign import mapAsyncWithOffsetImpl
  :: EffectFn3 GPUBuffer GPUMapMode GPUSize64 (Promise Unit)

mapAsyncWithOffset
  :: GPUBuffer -> GPUMapMode -> GPUSize64 -> Effect (Promise Unit)
mapAsyncWithOffset a b c = runEffectFn3 mapAsyncWithOffsetImpl a b c

foreign import mapAsyncWithSizeImpl
  :: EffectFn3 GPUBuffer GPUMapMode GPUSize64 (Promise Unit)

mapAsyncWithSize
  :: GPUBuffer -> GPUMapMode -> GPUSize64 -> Effect (Promise Unit)
mapAsyncWithSize a b c = runEffectFn3 mapAsyncWithSizeImpl a b c

foreign import mapAsyncWithOffsetAndSizeImpl
  :: EffectFn4 GPUBuffer GPUMapMode GPUSize64 GPUSize64 (Promise Unit)

mapAsyncWithOffsetAndSize
  :: GPUBuffer -> GPUMapMode -> GPUSize64 -> GPUSize64 -> Effect (Promise Unit)
mapAsyncWithOffsetAndSize a b c d = runEffectFn4 mapAsyncWithOffsetAndSizeImpl a
  b
  c
  d

foreign import getMappedRangeImpl :: EffectFn1 GPUBuffer ArrayBuffer

getMappedRange :: GPUBuffer -> Effect ArrayBuffer
getMappedRange a = runEffectFn1 getMappedRangeImpl a

foreign import getMappedRangeWithOffsetImpl
  :: EffectFn2 GPUBuffer GPUSize64 ArrayBuffer

getMappedRangeWithOffset :: GPUBuffer -> GPUSize64 -> Effect ArrayBuffer
getMappedRangeWithOffset a b = runEffectFn2 getMappedRangeWithOffsetImpl a b

foreign import getMappedRangeWithSizeImpl
  :: EffectFn2 GPUBuffer GPUSize64 ArrayBuffer

getMappedRangeWithSize :: GPUBuffer -> GPUSize64 -> Effect ArrayBuffer
getMappedRangeWithSize a b = runEffectFn2 getMappedRangeWithSizeImpl a b

foreign import getMappedRangeWithOffsetAndSizeImpl
  :: EffectFn3 GPUBuffer GPUSize64 GPUSize64 ArrayBuffer

getMappedRangeWithOffsetAndSize
  :: GPUBuffer -> GPUSize64 -> GPUSize64 -> Effect ArrayBuffer
getMappedRangeWithOffsetAndSize a b c = runEffectFn3
  getMappedRangeWithOffsetAndSizeImpl
  a
  b
  c

foreign import unmapImpl :: EffectFn1 GPUBuffer Unit

unmap :: GPUBuffer -> Effect Unit
unmap a = runEffectFn1 unmapImpl a

foreign import destroyImpl :: EffectFn1 GPUBuffer Unit

destroy :: GPUBuffer -> Effect Unit
destroy a = runEffectFn1 destroyImpl a
