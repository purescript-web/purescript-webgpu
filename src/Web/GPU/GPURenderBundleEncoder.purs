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
