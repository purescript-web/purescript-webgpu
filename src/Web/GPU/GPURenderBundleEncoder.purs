-- @inline export finishImpl arity=2
-- @inline export setPipelineImpl arity=2
-- @inline export setPipeline arity=2
-- @inline export setIndexBufferImpl arity=3
-- @inline export setIndexBuffer arity=3
-- @inline export setIndexBufferWithSizeImpl arity=4
-- @inline export setIndexBufferWithSize arity=4
-- @inline export setIndexBufferWithOffsetImpl arity=4
-- @inline export setIndexBufferWithOffset arity=4
-- @inline export setIndexBufferWithOffsetAndSizeImpl arity=5
-- @inline export setIndexBufferWithOffsetAndSize arity=5
-- @inline export setVertexBufferImpl arity=3
-- @inline export setVertexBuffer arity=3
-- @inline export setVertexBufferWithOffsetImpl arity=4
-- @inline export setVertexBufferWithOffset arity=4
-- @inline export setVertexBufferWithSizeImpl arity=4
-- @inline export setVertexBufferWithSize arity=4
-- @inline export setVertexBufferWithOffsetAndSizeImpl arity=5
-- @inline export setVertexBufferWithOffsetAndSize arity=5
-- @inline export drawImpl arity=2
-- @inline export draw arity=2
-- @inline export drawWithInstanceCountImpl arity=3
-- @inline export drawWithInstanceCount arity=3
-- @inline export drawWithFirstVertexImpl arity=2
-- @inline export drawWithFirstVertex arity=2
-- @inline export drawWithFirstInstanceImpl arity=2
-- @inline export drawWithFirstInstance arity=2
-- @inline export drawWithInstanceCountAndFirstVertexImpl arity=4
-- @inline export drawWithInstanceCountAndFirstVertex arity=4
-- @inline export drawWithInstanceCountAndFirstInstanceImpl arity=4
-- @inline export drawWithInstanceCountAndFirstInstance arity=4
-- @inline export drawWithFirstVertexAndFirstInstanceImpl arity=4
-- @inline export drawWithFirstVertexAndFirstInstance arity=4
-- @inline export drawWithInstanceCountAndFirstVertexAndFirstInstanceImpl arity=5
-- @inline export drawWithInstanceCountAndFirstVertexAndFirstInstance arity=5
-- @inline export drawIndexedImpl arity=2
-- @inline export drawIndexed arity=2
-- @inline export drawIndexedWithInstanceCountImpl arity=3
-- @inline export drawIndexedWithInstanceCount arity=3
-- @inline export drawIndexedWithFirstIndexImpl arity=3
-- @inline export drawIndexedWithFirstIndex arity=3
-- @inline export drawIndexedWithBaseVertexImpl arity=3
-- @inline export drawIndexedWithBaseVertex arity=3
-- @inline export drawIndexedWithFirstInstanceImpl arity=3
-- @inline export drawIndexedWithFirstInstance arity=3
-- @inline export drawIndexedWithInstanceCountAndFirstIndexImpl arity=4
-- @inline export drawIndexedWithInstanceCountAndFirstIndex arity=4
-- @inline export drawIndexedWithInstanceCountAndBaseVertexImpl arity=4
-- @inline export drawIndexedWithInstanceCountAndBaseVertex arity=4
-- @inline export drawIndexedWithInstanceCountAndFirstInstanceImpl arity=4
-- @inline export drawIndexedWithInstanceCountAndFirstInstance arity=4
-- @inline export drawIndexedWithFirstIndexAndBaseVertexImpl arity=4
-- @inline export drawIndexedWithFirstIndexAndBaseVertex arity=4
-- @inline export drawIndexedWithFirstIndexAndFirstInstanceImpl arity=4
-- @inline export drawIndexedWithFirstIndexAndFirstInstance arity=4
-- @inline export drawIndexedWithBaseVertexAndFirstInstanceImpl arity=4
-- @inline export drawIndexedWithBaseVertexAndFirstInstance arity=4
-- @inline export drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexImpl arity=5
-- @inline export drawIndexedWithInstanceCountAndFirstIndexAndBaseVertex arity=5
-- @inline export drawIndexedWithInstanceCountAndFirstIndexAndFirstInstanceImpl arity=5
-- @inline export drawIndexedWithInstanceCountAndFirstIndexAndFirstInstance arity=5
-- @inline export drawIndexedWithFirstIndexAndBaseVertexAndFirstInstanceImpl arity=5
-- @inline export drawIndexedWithFirstIndexAndBaseVertexAndFirstInstance arity=5
-- @inline export drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstanceImpl arity=6
-- @inline export drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstance arity=6
-- @inline export drawIndirectImpl arity=3
-- @inline export drawIndirect arity=3
-- @inline export drawIndexedIndirectImpl arity=3
-- @inline export drawIndexedIndirect arity=3
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
module Web.GPU.GPURenderBundleEncoder
  ( draw
  , drawIndexed
  , drawIndexedIndirect
  , drawIndexedWithBaseVertex
  , drawIndexedWithBaseVertexAndFirstInstance
  , drawIndexedWithFirstIndex
  , drawIndexedWithFirstIndexAndBaseVertex
  , drawIndexedWithFirstIndexAndBaseVertexAndFirstInstance
  , drawIndexedWithFirstIndexAndFirstInstance
  , drawIndexedWithFirstInstance
  , drawIndexedWithInstanceCount
  , drawIndexedWithInstanceCountAndBaseVertex
  , drawIndexedWithInstanceCountAndFirstIndex
  , drawIndexedWithInstanceCountAndFirstIndexAndBaseVertex
  , drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstance
  , drawIndexedWithInstanceCountAndFirstIndexAndFirstInstance
  , drawIndexedWithInstanceCountAndFirstInstance
  , drawIndirect
  , drawWithFirstInstance
  , drawWithFirstVertex
  , drawWithFirstVertexAndFirstInstance
  , drawWithInstanceCount
  , drawWithInstanceCountAndFirstInstance
  , drawWithInstanceCountAndFirstVertex
  , drawWithInstanceCountAndFirstVertexAndFirstInstance
  , finish
  , insertDebugMarker
  , popDebugGroup
  , pushDebugGroup
  , setBindGroup
  , setBindGroupWithDyanmicOffsetBounds
  , setBindGroupWithDynamicOffsets
  , setIndexBuffer
  , setIndexBufferWithOffset
  , setIndexBufferWithOffsetAndSize
  , setIndexBufferWithSize
  , setPipeline
  , setVertexBuffer
  , setVertexBufferWithOffset
  , setVertexBufferWithOffsetAndSize
  , setVertexBufferWithSize
  ) where

import Prelude

import Data.ArrayBuffer.Types (Uint32Array)
import Effect (Effect)
import Web.GPU.GPUIndexFormat (GPUIndexFormat)
import Web.GPU.GPURenderBundleDescriptor (GPURenderBundleDescriptor)
import Web.GPU.Internal.Types (GPUBindGroup, GPUBuffer, GPUBufferDynamicOffset, GPUIndex32, GPURenderBundle, GPURenderBundleEncoder, GPURenderPipeline, GPUSignedOffset32, GPUSize32, GPUSize64)

foreign import finishImpl
  :: GPURenderBundleEncoder
  -> GPURenderBundleDescriptor
  -> Effect GPURenderBundle

finish
  ∷ GPURenderBundleEncoder -> GPURenderBundleDescriptor → Effect GPURenderBundle
finish = finishImpl

foreign import setPipelineImpl
  :: GPURenderBundleEncoder -> GPURenderPipeline -> Effect Unit

setPipeline :: GPURenderBundleEncoder -> GPURenderPipeline -> Effect Unit
setPipeline = setPipelineImpl

foreign import setIndexBufferImpl
  :: GPURenderBundleEncoder -> GPUBuffer -> GPUIndexFormat -> Effect Unit

setIndexBuffer
  :: GPURenderBundleEncoder -> GPUBuffer -> GPUIndexFormat -> Effect Unit
setIndexBuffer = setIndexBufferImpl

foreign import setIndexBufferWithSizeImpl
  :: GPURenderBundleEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> Effect Unit

setIndexBufferWithSize
  :: GPURenderBundleEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> Effect Unit
setIndexBufferWithSize = setIndexBufferWithSizeImpl

foreign import setIndexBufferWithOffsetImpl
  :: GPURenderBundleEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> Effect Unit

setIndexBufferWithOffset
  :: GPURenderBundleEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> Effect Unit
setIndexBufferWithOffset = setIndexBufferWithOffsetImpl

foreign import setIndexBufferWithOffsetAndSizeImpl
  :: GPURenderBundleEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit

setIndexBufferWithOffsetAndSize
  :: GPURenderBundleEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit
setIndexBufferWithOffsetAndSize = setIndexBufferWithOffsetAndSizeImpl

foreign import setVertexBufferImpl
  :: GPURenderBundleEncoder -> GPUIndex32 -> GPUBuffer -> Effect Unit

setVertexBuffer
  :: GPURenderBundleEncoder -> GPUIndex32 -> GPUBuffer -> Effect Unit
setVertexBuffer = setVertexBufferImpl

foreign import setVertexBufferWithOffsetImpl
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBuffer
  -> GPUSize64
  -> Effect Unit

setVertexBufferWithOffset
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBuffer
  -> GPUSize64
  -> Effect Unit
setVertexBufferWithOffset = setVertexBufferWithOffsetImpl

foreign import setVertexBufferWithSizeImpl
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBuffer
  -> GPUSize64
  -> Effect Unit

setVertexBufferWithSize
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBuffer
  -> GPUSize64
  -> Effect Unit
setVertexBufferWithSize = setVertexBufferWithSizeImpl

foreign import setVertexBufferWithOffsetAndSizeImpl
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBuffer
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit

setVertexBufferWithOffsetAndSize
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBuffer
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit
setVertexBufferWithOffsetAndSize = setVertexBufferWithOffsetAndSizeImpl

foreign import drawImpl :: GPURenderBundleEncoder -> GPUSize32 -> Effect Unit

draw :: GPURenderBundleEncoder -> GPUSize32 -> Effect Unit
draw = drawImpl

foreign import drawWithInstanceCountImpl
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit

drawWithInstanceCount
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawWithInstanceCount = drawWithInstanceCountImpl

foreign import drawWithFirstVertexImpl
  :: GPURenderBundleEncoder -> GPUSize32 -> Effect Unit

drawWithFirstVertex
  :: GPURenderBundleEncoder -> GPUSize32 -> Effect Unit
drawWithFirstVertex = drawWithFirstVertexImpl

foreign import drawWithFirstInstanceImpl
  :: GPURenderBundleEncoder -> GPUSize32 -> Effect Unit

drawWithFirstInstance
  :: GPURenderBundleEncoder -> GPUSize32 -> Effect Unit
drawWithFirstInstance = drawWithFirstInstanceImpl

foreign import drawWithInstanceCountAndFirstVertexImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit

drawWithInstanceCountAndFirstVertex
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawWithInstanceCountAndFirstVertex = drawWithInstanceCountAndFirstVertexImpl

foreign import drawWithInstanceCountAndFirstInstanceImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit

drawWithInstanceCountAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawWithInstanceCountAndFirstInstance =
  drawWithInstanceCountAndFirstInstanceImpl

foreign import drawWithFirstVertexAndFirstInstanceImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit

drawWithFirstVertexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawWithFirstVertexAndFirstInstance = drawWithFirstVertexAndFirstInstanceImpl

foreign import drawWithInstanceCountAndFirstVertexAndFirstInstanceImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit

drawWithInstanceCountAndFirstVertexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawWithInstanceCountAndFirstVertexAndFirstInstance =
  drawWithInstanceCountAndFirstVertexAndFirstInstanceImpl

foreign import drawIndexedImpl
  :: GPURenderBundleEncoder -> GPUSize32 -> Effect Unit

drawIndexed :: GPURenderBundleEncoder -> GPUSize32 -> Effect Unit
drawIndexed = drawIndexedImpl

foreign import drawIndexedWithInstanceCountImpl
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit

drawIndexedWithInstanceCount
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithInstanceCount = drawIndexedWithInstanceCountImpl

foreign import drawIndexedWithFirstIndexImpl
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit

drawIndexedWithFirstIndex
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithFirstIndex = drawIndexedWithFirstIndexImpl

foreign import drawIndexedWithBaseVertexImpl
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSignedOffset32 -> Effect Unit

drawIndexedWithBaseVertex
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSignedOffset32 -> Effect Unit
drawIndexedWithBaseVertex = drawIndexedWithBaseVertexImpl

foreign import drawIndexedWithFirstInstanceImpl
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit

drawIndexedWithFirstInstance
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithFirstInstance = drawIndexedWithFirstInstanceImpl

foreign import drawIndexedWithInstanceCountAndFirstIndexImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit

drawIndexedWithInstanceCountAndFirstIndex
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndex =
  drawIndexedWithInstanceCountAndFirstIndexImpl

foreign import drawIndexedWithInstanceCountAndBaseVertexImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit

drawIndexedWithInstanceCountAndBaseVertex
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit
drawIndexedWithInstanceCountAndBaseVertex =
  drawIndexedWithInstanceCountAndBaseVertexImpl

foreign import drawIndexedWithInstanceCountAndFirstInstanceImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit

drawIndexedWithInstanceCountAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstInstance =
  drawIndexedWithInstanceCountAndFirstInstanceImpl

foreign import drawIndexedWithFirstIndexAndBaseVertexImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit

drawIndexedWithFirstIndexAndBaseVertex
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit
drawIndexedWithFirstIndexAndBaseVertex =
  drawIndexedWithFirstIndexAndBaseVertexImpl

foreign import drawIndexedWithFirstIndexAndFirstInstanceImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit

drawIndexedWithFirstIndexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithFirstIndexAndFirstInstance =
  drawIndexedWithFirstIndexAndFirstInstanceImpl

foreign import drawIndexedWithBaseVertexAndFirstInstanceImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit

drawIndexedWithBaseVertexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithBaseVertexAndFirstInstance =
  drawIndexedWithBaseVertexAndFirstInstanceImpl

foreign import drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit

drawIndexedWithInstanceCountAndFirstIndexAndBaseVertex
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndexAndBaseVertex =
  drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexImpl

foreign import drawIndexedWithInstanceCountAndFirstIndexAndFirstInstanceImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit

drawIndexedWithInstanceCountAndFirstIndexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndexAndFirstInstance =
  drawIndexedWithInstanceCountAndFirstIndexAndFirstInstanceImpl

foreign import drawIndexedWithFirstIndexAndBaseVertexAndFirstInstanceImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit

drawIndexedWithFirstIndexAndBaseVertexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithFirstIndexAndBaseVertexAndFirstInstance =
  drawIndexedWithFirstIndexAndBaseVertexAndFirstInstanceImpl

foreign import drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstanceImpl
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit

drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstance =
  drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstanceImpl

foreign import drawIndirectImpl
  :: GPURenderBundleEncoder -> GPUBuffer -> GPUSize64 -> Effect Unit

drawIndirect :: GPURenderBundleEncoder -> GPUBuffer -> GPUSize64 -> Effect Unit
drawIndirect = drawIndirectImpl

foreign import drawIndexedIndirectImpl
  :: GPURenderBundleEncoder -> GPUBuffer -> GPUSize64 -> Effect Unit

drawIndexedIndirect
  :: GPURenderBundleEncoder -> GPUBuffer -> GPUSize64 -> Effect Unit
drawIndexedIndirect = drawIndexedIndirectImpl

foreign import setBindGroupImpl
  :: GPURenderBundleEncoder -> GPUIndex32 -> GPUBindGroup -> Effect Unit

setBindGroup
  :: GPURenderBundleEncoder -> GPUIndex32 -> GPUBindGroup -> Effect Unit
setBindGroup = setBindGroupImpl

foreign import setBindGroupWithDynamicOffsetsImpl
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Array GPUBufferDynamicOffset
  -> Effect Unit

setBindGroupWithDynamicOffsets
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Array GPUBufferDynamicOffset
  -> Effect Unit
setBindGroupWithDynamicOffsets = setBindGroupWithDynamicOffsetsImpl

foreign import setBindGroupWithDyanmicOffsetBoundsImpl
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Uint32Array
  -> GPUSize64
  -> GPUSize32
  -> Effect Unit

setBindGroupWithDyanmicOffsetBounds
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Uint32Array
  -> GPUSize64
  -> GPUSize32
  -> Effect Unit
setBindGroupWithDyanmicOffsetBounds = setBindGroupWithDyanmicOffsetBoundsImpl

foreign import pushDebugGroupImpl
  :: GPURenderBundleEncoder -> String -> Effect Unit

pushDebugGroup :: GPURenderBundleEncoder -> String -> Effect Unit
pushDebugGroup = pushDebugGroupImpl

foreign import popDebugGroupImpl :: GPURenderBundleEncoder -> Effect Unit

popDebugGroup :: GPURenderBundleEncoder -> Effect Unit
popDebugGroup = popDebugGroupImpl

foreign import insertDebugMarkerImpl
  :: GPURenderBundleEncoder -> String -> Effect Unit

insertDebugMarker :: GPURenderBundleEncoder -> String -> Effect Unit
insertDebugMarker = insertDebugMarkerImpl