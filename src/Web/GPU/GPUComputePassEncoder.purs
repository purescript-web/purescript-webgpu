-- @inline export setPipelineImpl arity=2
-- @inline export dispatchWorkgroupsImpl arity=2
-- @inline export dispatchWorkgroupsXImpl arity=2
-- @inline export dispatchWorkgroupsXYImpl arity=3
-- @inline export dispatchWorkgroupsXY arity=3
-- @inline export dispatchWorkgroupsXYZImpl arity=4
-- @inline export dispatchWorkgroupsXYZ arity=4
-- @inline export dispatchWorkgroupsIndirectImpl arity=3
-- @inline export dispatchWorkgroupsIndirect arity=3
-- @inline export endImpl arity=1
-- @inline export end arity=1
-- @inline export setBindGroupImpl arity=3
-- @inline export setBindGroup arity=3
-- @inline export setBindGroupWithDynamicOffsetsImpl arity=4
-- @inline export setBindGroupWithDynamicOffsets arity=4
-- @inline export setBindGroupWithDyanmicOffsetBoundsImpl arity=6
-- @inline export setBindGroupWithDyanmicOffsetBounds arity=6
-- @inline export pushDebugGroupImpl arity=2
-- @inline export pushDebugGroup arity=2
-- @inline export popDebugGroupImpl arity=1
-- @inline export popDebugGroup arity=1
-- @inline export insertDebugMarkerImpl arity=2
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
import Web.GPU.GPUBindGroup (GPUBindGroup)
import Web.GPU.GPUBuffer (GPUBuffer)
import Web.GPU.GPUComputePipeline (GPUComputePipeline)
import Web.GPU.Internal.Types (GPUIndex32, GPUSize32, GPUSize64, GPUBufferDynamicOffset)

data GPUComputePassEncoder

foreign import setPipelineImpl
  :: GPUComputePassEncoder -> GPUComputePipeline -> Effect Unit

setPipeline ∷ GPUComputePassEncoder → GPUComputePipeline → Effect Unit
setPipeline = setPipelineImpl

foreign import dispatchWorkgroupsImpl
  :: GPUComputePassEncoder -> GPUSize32 -> Effect Unit

dispatchWorkgroups ∷ GPUComputePassEncoder → Int → Effect Unit
dispatchWorkgroups = dispatchWorkgroupsImpl

foreign import dispatchWorkgroupsXImpl
  :: GPUComputePassEncoder -> GPUSize32 -> Effect Unit

dispatchWorkgroupsX ∷ GPUComputePassEncoder → Int → Effect Unit
dispatchWorkgroupsX = dispatchWorkgroupsXImpl

foreign import dispatchWorkgroupsXYImpl
  :: GPUComputePassEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit

dispatchWorkgroupsXY :: GPUComputePassEncoder -> Int -> Int -> Effect Unit
dispatchWorkgroupsXY = dispatchWorkgroupsXYImpl

foreign import dispatchWorkgroupsXYZImpl
  :: GPUComputePassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit

dispatchWorkgroupsXYZ
  :: GPUComputePassEncoder -> Int -> Int -> Int -> Effect Unit
dispatchWorkgroupsXYZ = dispatchWorkgroupsXYZImpl

foreign import dispatchWorkgroupsIndirectImpl
  :: GPUComputePassEncoder -> GPUBuffer -> GPUSize64 -> Effect Unit

dispatchWorkgroupsIndirect
  :: GPUComputePassEncoder -> GPUBuffer -> Int -> Effect Unit
dispatchWorkgroupsIndirect = dispatchWorkgroupsIndirectImpl

foreign import endImpl :: GPUComputePassEncoder -> Effect Unit

end :: GPUComputePassEncoder -> Effect Unit
end = endImpl

foreign import setBindGroupImpl
  :: GPUComputePassEncoder -> GPUIndex32 -> GPUBindGroup -> Effect Unit

setBindGroup :: GPUComputePassEncoder -> Int -> GPUBindGroup -> Effect Unit
setBindGroup = setBindGroupImpl

foreign import setBindGroupWithDynamicOffsetsImpl
  :: GPUComputePassEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Array GPUBufferDynamicOffset
  -> Effect Unit

setBindGroupWithDynamicOffsets
  :: GPUComputePassEncoder -> Int -> GPUBindGroup -> Array Int -> Effect Unit
setBindGroupWithDynamicOffsets = setBindGroupWithDynamicOffsetsImpl

foreign import setBindGroupWithDyanmicOffsetBoundsImpl
  :: GPUComputePassEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Uint32Array
  -> GPUSize64
  -> GPUSize32
  -> Effect Unit

setBindGroupWithDyanmicOffsetBounds
  :: GPUComputePassEncoder
  -> Int
  -> GPUBindGroup
  -> Uint32Array
  -> Int
  -> Int
  -> Effect Unit
setBindGroupWithDyanmicOffsetBounds = setBindGroupWithDyanmicOffsetBoundsImpl

foreign import pushDebugGroupImpl
  :: GPUComputePassEncoder -> String -> Effect Unit

pushDebugGroup :: GPUComputePassEncoder -> String -> Effect Unit
pushDebugGroup = pushDebugGroupImpl

foreign import popDebugGroupImpl :: GPUComputePassEncoder -> Effect Unit

popDebugGroup :: GPUComputePassEncoder -> Effect Unit
popDebugGroup = popDebugGroupImpl

foreign import insertDebugMarkerImpl
  :: GPUComputePassEncoder -> String -> Effect Unit

insertDebugMarker :: GPUComputePassEncoder -> String -> Effect Unit
insertDebugMarker = insertDebugMarkerImpl