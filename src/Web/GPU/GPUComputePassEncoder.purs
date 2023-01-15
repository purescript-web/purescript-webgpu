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
  )
  where

import Prelude

import Data.ArrayBuffer.Types (Uint32Array)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn5, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn5)
import Web.GPU.GPUBindGroup (GPUBindGroup)
import Web.GPU.GPUBuffer (GPUBuffer)
import Web.GPU.GPUComputePipeline (GPUComputePipeline)
import Web.GPU.Internal.Types (GPUIndex32, GPUSize32, GPUSize64, GPUBufferDynamicOffset)

data GPUComputePassEncoder

foreign import setPipelineImpl
  :: GPUComputePassEncoder -> EffectFn1 GPUComputePipeline Unit

setPipeline ∷ GPUComputePassEncoder → GPUComputePipeline → Effect Unit
setPipeline a b = runEffectFn1 (setPipelineImpl a) b

foreign import dispatchWorkgroupsImpl
  :: GPUComputePassEncoder -> EffectFn1 GPUSize32 Unit

dispatchWorkgroups ∷ GPUComputePassEncoder → Int → Effect Unit
dispatchWorkgroups a b = runEffectFn1 (dispatchWorkgroupsImpl a) b

foreign import dispatchWorkgroupsXImpl
  :: GPUComputePassEncoder -> EffectFn1 GPUSize32 Unit

dispatchWorkgroupsX ∷ GPUComputePassEncoder → Int → Effect Unit
dispatchWorkgroupsX a b = runEffectFn1 (dispatchWorkgroupsXImpl a) b

foreign import dispatchWorkgroupsXYImpl
  :: GPUComputePassEncoder -> EffectFn2 GPUSize32 GPUSize32 Unit

dispatchWorkgroupsXY :: GPUComputePassEncoder -> Int -> Int -> Effect Unit
dispatchWorkgroupsXY a b c = runEffectFn2 (dispatchWorkgroupsXYImpl a) b c

foreign import dispatchWorkgroupsXYZImpl
  :: GPUComputePassEncoder -> EffectFn3 GPUSize32 GPUSize32 GPUSize32 Unit

dispatchWorkgroupsXYZ
  :: GPUComputePassEncoder -> Int -> Int -> Int -> Effect Unit
dispatchWorkgroupsXYZ a b c d = runEffectFn3 (dispatchWorkgroupsXYZImpl a) b c d

foreign import dispatchWorkgroupsIndirectImpl
  :: GPUComputePassEncoder -> EffectFn2 GPUBuffer GPUSize64 Unit

dispatchWorkgroupsIndirect
  :: GPUComputePassEncoder -> GPUBuffer -> Int -> Effect Unit
dispatchWorkgroupsIndirect a b c = runEffectFn2 (dispatchWorkgroupsIndirectImpl a) b c

foreign import endImpl :: GPUComputePassEncoder -> Effect Unit

end :: GPUComputePassEncoder -> Effect Unit
end = endImpl

foreign import setBindGroupImpl
  :: GPUComputePassEncoder -> EffectFn2 GPUIndex32 GPUBindGroup Unit

setBindGroup :: GPUComputePassEncoder -> Int -> GPUBindGroup -> Effect Unit
setBindGroup a b c = runEffectFn2 (setBindGroupImpl a) b c

foreign import setBindGroupWithDynamicOffsetsImpl
  :: GPUComputePassEncoder
  -> EffectFn3 GPUIndex32 GPUBindGroup (Array GPUBufferDynamicOffset) Unit

setBindGroupWithDynamicOffsets
  :: GPUComputePassEncoder -> Int -> GPUBindGroup -> Array Int -> Effect Unit
setBindGroupWithDynamicOffsets a b c d = runEffectFn3 (setBindGroupWithDynamicOffsetsImpl a) b c d

foreign import setBindGroupWithDyanmicOffsetBoundsImpl
  :: GPUComputePassEncoder
  -> EffectFn5 GPUIndex32 GPUBindGroup Uint32Array GPUSize64 GPUSize32 Unit

setBindGroupWithDyanmicOffsetBounds
  :: GPUComputePassEncoder
  -> Int
  -> GPUBindGroup
  -> Uint32Array
  -> Int
  -> Int
  -> Effect Unit
setBindGroupWithDyanmicOffsetBounds a b c d e f = runEffectFn5 (setBindGroupWithDyanmicOffsetBoundsImpl a) b c d e f

foreign import pushDebugGroupImpl
  :: GPUComputePassEncoder -> EffectFn1 String Unit

pushDebugGroup :: GPUComputePassEncoder -> String -> Effect Unit
pushDebugGroup a b = runEffectFn1 (pushDebugGroupImpl a) b

foreign import popDebugGroupImpl :: GPUComputePassEncoder -> Effect Unit

popDebugGroup :: GPUComputePassEncoder -> Effect Unit
popDebugGroup = popDebugGroupImpl

foreign import insertDebugMarkerImpl
  :: GPUComputePassEncoder -> EffectFn1 String Unit

insertDebugMarker :: GPUComputePassEncoder -> String -> Effect Unit
insertDebugMarker a b = runEffectFn1 (insertDebugMarkerImpl a) b