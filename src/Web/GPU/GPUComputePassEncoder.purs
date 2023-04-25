-- @inline export dispatchWorkgroupsXY arity=3
-- @inline export dispatchWorkgroupsXYZ arity=4
-- @inline export dispatchWorkgroupsIndirect arity=3
-- @inline export end arity=1
-- @inline export setBindGroup arity=3
-- @inline export setBindGroupWithDynamicOffsets arity=4
-- @inline export setBindGroupWithDyanmicOffsetBounds arity=6
-- @inline export pushDebugGroup arity=2
-- @inline export popDebugGroup arity=1
-- @inline export insertDebugMarker arity=2
module Web.GPU.GPUComputePassEncoder
  ( GPUComputePassEncoder(..)
  , dispatchWorkgroups
  , dispatchWorkgroupsIndirect
  , dispatchWorkgroupsX
  , dispatchWorkgroupsXY
  , dispatchWorkgroupsXYZ
  , end
  , insertDebugMarker
  , popDebugGroup
  , pushDebugGroup
  , setBindGroup
  , setBindGroupWithDyanmicOffsetBounds
  , setBindGroupWithDynamicOffsets
  , setPipeline
  ) where

import Prelude
import Data.ArrayBuffer.Types (Uint32Array)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn6, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn6)
import Web.GPU.GPUBindGroup (GPUBindGroup)
import Web.GPU.GPUBuffer (GPUBuffer)
import Web.GPU.GPUComputePipeline (GPUComputePipeline)
import Web.GPU.Internal.Types (GPUIndex32, GPUSize32, GPUSize64, GPUBufferDynamicOffset)

data GPUComputePassEncoder

foreign import setPipelineImpl
  :: EffectFn2 GPUComputePassEncoder GPUComputePipeline Unit

setPipeline ∷ GPUComputePassEncoder → GPUComputePipeline → Effect Unit
setPipeline a b = runEffectFn2 setPipelineImpl a b

foreign import dispatchWorkgroupsImpl
  ::EffectFn2 GPUComputePassEncoder GPUSize32 Unit

dispatchWorkgroups ∷ GPUComputePassEncoder → Int → Effect Unit
dispatchWorkgroups a b = runEffectFn2 dispatchWorkgroupsImpl a b

foreign import dispatchWorkgroupsXImpl
  ::  EffectFn2 GPUComputePassEncoder GPUSize32 Unit

dispatchWorkgroupsX ∷ GPUComputePassEncoder → Int → Effect Unit
dispatchWorkgroupsX a b = runEffectFn2 dispatchWorkgroupsXImpl a b

foreign import dispatchWorkgroupsXYImpl
  :: EffectFn3 GPUComputePassEncoder GPUSize32 GPUSize32 Unit

dispatchWorkgroupsXY :: GPUComputePassEncoder -> Int -> Int -> Effect Unit
dispatchWorkgroupsXY a b c = runEffectFn3 dispatchWorkgroupsXYImpl a b c

foreign import dispatchWorkgroupsXYZImpl
  :: EffectFn4 GPUComputePassEncoder GPUSize32 GPUSize32 GPUSize32 Unit

dispatchWorkgroupsXYZ
  :: GPUComputePassEncoder -> Int -> Int -> Int -> Effect Unit
dispatchWorkgroupsXYZ a b c d = runEffectFn4 dispatchWorkgroupsXYZImpl a b c d

foreign import dispatchWorkgroupsIndirectImpl
  :: EffectFn3 GPUComputePassEncoder GPUBuffer GPUSize64 Unit

dispatchWorkgroupsIndirect
  :: GPUComputePassEncoder -> GPUBuffer -> Int -> Effect Unit
dispatchWorkgroupsIndirect a b c = runEffectFn3
  dispatchWorkgroupsIndirectImpl
  a
  b
  c

foreign import endImpl :: EffectFn1 GPUComputePassEncoder  Unit
end :: GPUComputePassEncoder -> Effect Unit
end a = runEffectFn1 endImpl a

foreign import setBindGroupImpl
  :: EffectFn3 GPUComputePassEncoder GPUIndex32 GPUBindGroup Unit

setBindGroup :: GPUComputePassEncoder -> Int -> GPUBindGroup -> Effect Unit
setBindGroup a b c = runEffectFn3 setBindGroupImpl a b c

foreign import setBindGroupWithDynamicOffsetsImpl
  :: EffectFn4 GPUComputePassEncoder
     GPUIndex32 GPUBindGroup (Array GPUBufferDynamicOffset) Unit

setBindGroupWithDynamicOffsets
  :: GPUComputePassEncoder -> Int -> GPUBindGroup -> Array Int -> Effect Unit
setBindGroupWithDynamicOffsets a b c d = runEffectFn4
  setBindGroupWithDynamicOffsetsImpl
  a
  b
  c
  d

foreign import setBindGroupWithDyanmicOffsetBoundsImpl
  :: EffectFn6 GPUComputePassEncoder
     GPUIndex32 GPUBindGroup Uint32Array GPUSize64 GPUSize32 Unit

setBindGroupWithDyanmicOffsetBounds
  :: GPUComputePassEncoder
  -> Int
  -> GPUBindGroup
  -> Uint32Array
  -> Int
  -> Int
  -> Effect Unit
setBindGroupWithDyanmicOffsetBounds a b c d e f = runEffectFn6
  setBindGroupWithDyanmicOffsetBoundsImpl
  a
  b
  c
  d
  e
  f

foreign import pushDebugGroupImpl
  :: EffectFn2 GPUComputePassEncoder String Unit

pushDebugGroup :: GPUComputePassEncoder -> String -> Effect Unit
pushDebugGroup a b = runEffectFn2 pushDebugGroupImpl a b

foreign import popDebugGroupImpl :: EffectFn1 GPUComputePassEncoder  Unit
popDebugGroup :: GPUComputePassEncoder -> Effect Unit
popDebugGroup a = runEffectFn1 popDebugGroupImpl a

foreign import insertDebugMarkerImpl
  :: EffectFn2 GPUComputePassEncoder String Unit

insertDebugMarker :: GPUComputePassEncoder -> String -> Effect Unit
insertDebugMarker a b = runEffectFn2 insertDebugMarkerImpl a b