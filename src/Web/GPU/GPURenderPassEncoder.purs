module Web.GPU.GPURenderPassEncoder
  ( beginOcclusionQuery
  , draw
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
  , end
  , endOcclusionQuery
  , executeBundles
  , insertDebugMarker
  , popDebugGroup
  , pushDebugGroup
  , setBindGroup
  , setBindGroupWithDyanmicOffsetBounds
  , setBindGroupWithDynamicOffsets
  , setBlendConstant
  , setIndexBuffer
  , setIndexBufferWithOffset
  , setIndexBufferWithOffsetAndSize
  , setIndexBufferWithSize
  , setPipeline
  , setScissorRect
  , setStencilReference
  , setVertexBuffer
  , setVertexBufferWithOffset
  , setVertexBufferWithOffsetAndSize
  , setVertexBufferWithSize
  , setViewport
  ) where

import Prelude

import Data.ArrayBuffer.Types (Uint32Array)
import Effect (Effect)
import Web.GPU.GPUIndexFormat (GPUIndexFormat)
import Web.GPU.Internal.Types (GPUBindGroup, GPUBuffer, GPUColor, GPUIndex32, GPUIntegerCoordinate, GPURenderBundle, GPURenderPassEncoder, GPURenderPipeline, GPUSignedOffset32, GPUSize32, GPUSize64, GPUStencilValue, GPUBufferDynamicOffset)

foreign import setViewportImpl
  :: GPURenderPassEncoder
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Effect Unit

setViewport
  :: GPURenderPassEncoder
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Effect Unit
setViewport = setViewportImpl

foreign import setScissorRectImpl
  :: GPURenderPassEncoder
  -> GPUIntegerCoordinate
  -> GPUIntegerCoordinate
  -> GPUIntegerCoordinate
  -> GPUIntegerCoordinate
  -> Effect Unit

setScissorRect
  :: GPURenderPassEncoder
  -> GPUIntegerCoordinate
  -> GPUIntegerCoordinate
  -> GPUIntegerCoordinate
  -> GPUIntegerCoordinate
  -> Effect Unit
setScissorRect = setScissorRectImpl

foreign import setBlendConstantImpl
  :: GPURenderPassEncoder -> GPUColor -> Effect Unit

setBlendConstant :: GPURenderPassEncoder -> GPUColor -> Effect Unit
setBlendConstant = setBlendConstantImpl

foreign import setStencilReferenceImpl
  :: GPURenderPassEncoder -> GPUStencilValue -> Effect Unit

setStencilReference :: GPURenderPassEncoder -> GPUStencilValue -> Effect Unit
setStencilReference = setStencilReferenceImpl

foreign import beginOcclusionQueryImpl
  :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit

beginOcclusionQuery :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit
beginOcclusionQuery = beginOcclusionQueryImpl

foreign import endOcclusionQueryImpl :: GPURenderPassEncoder -> Effect Unit

endOcclusionQuery :: GPURenderPassEncoder -> Effect Unit
endOcclusionQuery = endOcclusionQueryImpl

foreign import executeBundlesImpl
  :: GPURenderPassEncoder -> Array GPURenderBundle -> Effect Unit

executeBundles :: GPURenderPassEncoder -> Array GPURenderBundle -> Effect Unit
executeBundles = executeBundlesImpl

foreign import endImpl :: GPURenderPassEncoder -> Effect Unit

end :: GPURenderPassEncoder -> Effect Unit
end = endImpl

foreign import setPipelineImpl
  :: GPURenderPassEncoder -> GPURenderPipeline -> Effect Unit

setPipeline :: GPURenderPassEncoder -> GPURenderPipeline -> Effect Unit
setPipeline = setPipelineImpl

foreign import setIndexBufferImpl
  :: GPURenderPassEncoder -> GPUBuffer -> GPUIndexFormat -> Effect Unit

setIndexBuffer
  :: GPURenderPassEncoder -> GPUBuffer -> GPUIndexFormat -> Effect Unit
setIndexBuffer = setIndexBufferImpl

foreign import setIndexBufferWithSizeImpl
  :: GPURenderPassEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> Effect Unit

setIndexBufferWithSize
  :: GPURenderPassEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> Effect Unit
setIndexBufferWithSize = setIndexBufferWithSizeImpl

foreign import setIndexBufferWithOffsetImpl
  :: GPURenderPassEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> Effect Unit

setIndexBufferWithOffset
  :: GPURenderPassEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> Effect Unit
setIndexBufferWithOffset = setIndexBufferWithOffsetImpl

foreign import setIndexBufferWithOffsetAndSizeImpl
  :: GPURenderPassEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit

setIndexBufferWithOffsetAndSize
  :: GPURenderPassEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit
setIndexBufferWithOffsetAndSize = setIndexBufferWithOffsetAndSizeImpl

foreign import setVertexBufferImpl
  :: GPURenderPassEncoder -> GPUIndex32 -> GPUBuffer -> Effect Unit

setVertexBuffer
  :: GPURenderPassEncoder -> GPUIndex32 -> GPUBuffer -> Effect Unit
setVertexBuffer = setVertexBufferImpl

foreign import setVertexBufferWithOffsetImpl
  :: GPURenderPassEncoder -> GPUIndex32 -> GPUBuffer -> GPUSize64 -> Effect Unit

setVertexBufferWithOffset
  :: GPURenderPassEncoder -> GPUIndex32 -> GPUBuffer -> GPUSize64 -> Effect Unit
setVertexBufferWithOffset = setVertexBufferWithOffsetImpl

foreign import setVertexBufferWithSizeImpl
  :: GPURenderPassEncoder -> GPUIndex32 -> GPUBuffer -> GPUSize64 -> Effect Unit

setVertexBufferWithSize
  :: GPURenderPassEncoder -> GPUIndex32 -> GPUBuffer -> GPUSize64 -> Effect Unit
setVertexBufferWithSize = setVertexBufferWithSizeImpl

foreign import setVertexBufferWithOffsetAndSizeImpl
  :: GPURenderPassEncoder
  -> GPUIndex32
  -> GPUBuffer
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit

setVertexBufferWithOffsetAndSize
  :: GPURenderPassEncoder
  -> GPUIndex32
  -> GPUBuffer
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit
setVertexBufferWithOffsetAndSize = setVertexBufferWithOffsetAndSizeImpl

foreign import drawImpl :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit

draw :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit
draw = drawImpl

foreign import drawWithInstanceCountImpl
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit

drawWithInstanceCount
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawWithInstanceCount = drawWithInstanceCountImpl

foreign import drawWithFirstVertexImpl
  :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit

drawWithFirstVertex
  :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit
drawWithFirstVertex = drawWithFirstVertexImpl

foreign import drawWithFirstInstanceImpl
  :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit

drawWithFirstInstance
  :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit
drawWithFirstInstance = drawWithFirstInstanceImpl

foreign import drawWithInstanceCountAndFirstVertexImpl
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit

drawWithInstanceCountAndFirstVertex
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit
drawWithInstanceCountAndFirstVertex = drawWithInstanceCountAndFirstVertexImpl

foreign import drawWithInstanceCountAndFirstInstanceImpl
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit

drawWithInstanceCountAndFirstInstance
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit
drawWithInstanceCountAndFirstInstance =
  drawWithInstanceCountAndFirstInstanceImpl

foreign import drawWithFirstVertexAndFirstInstanceImpl
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit

drawWithFirstVertexAndFirstInstance
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit
drawWithFirstVertexAndFirstInstance = drawWithFirstVertexAndFirstInstanceImpl

foreign import drawWithInstanceCountAndFirstVertexAndFirstInstanceImpl
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit

drawWithInstanceCountAndFirstVertexAndFirstInstance
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawWithInstanceCountAndFirstVertexAndFirstInstance =
  drawWithInstanceCountAndFirstVertexAndFirstInstanceImpl

foreign import drawIndexedImpl
  :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit

drawIndexed :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit
drawIndexed = drawIndexedImpl

foreign import drawIndexedWithInstanceCountImpl
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit

drawIndexedWithInstanceCount
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithInstanceCount = drawIndexedWithInstanceCountImpl

foreign import drawIndexedWithFirstIndexImpl
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit

drawIndexedWithFirstIndex
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithFirstIndex = drawIndexedWithFirstIndexImpl

foreign import drawIndexedWithBaseVertexImpl
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSignedOffset32 -> Effect Unit

drawIndexedWithBaseVertex
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSignedOffset32 -> Effect Unit
drawIndexedWithBaseVertex = drawIndexedWithBaseVertexImpl

foreign import drawIndexedWithFirstInstanceImpl
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit

drawIndexedWithFirstInstance
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithFirstInstance = drawIndexedWithFirstInstanceImpl

foreign import drawIndexedWithInstanceCountAndFirstIndexImpl
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit

drawIndexedWithInstanceCountAndFirstIndex
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndex =
  drawIndexedWithInstanceCountAndFirstIndexImpl

foreign import drawIndexedWithInstanceCountAndBaseVertexImpl
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit

drawIndexedWithInstanceCountAndBaseVertex
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit
drawIndexedWithInstanceCountAndBaseVertex =
  drawIndexedWithInstanceCountAndBaseVertexImpl

foreign import drawIndexedWithInstanceCountAndFirstInstanceImpl
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit

drawIndexedWithInstanceCountAndFirstInstance
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithInstanceCountAndFirstInstance =
  drawIndexedWithInstanceCountAndFirstInstanceImpl

foreign import drawIndexedWithFirstIndexAndBaseVertexImpl
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit

drawIndexedWithFirstIndexAndBaseVertex
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit
drawIndexedWithFirstIndexAndBaseVertex =
  drawIndexedWithFirstIndexAndBaseVertexImpl

foreign import drawIndexedWithFirstIndexAndFirstInstanceImpl
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit

drawIndexedWithFirstIndexAndFirstInstance
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithFirstIndexAndFirstInstance =
  drawIndexedWithFirstIndexAndFirstInstanceImpl

foreign import drawIndexedWithBaseVertexAndFirstInstanceImpl
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit

drawIndexedWithBaseVertexAndFirstInstance
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithBaseVertexAndFirstInstance =
  drawIndexedWithBaseVertexAndFirstInstanceImpl

foreign import drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexImpl
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit

drawIndexedWithInstanceCountAndFirstIndexAndBaseVertex
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndexAndBaseVertex =
  drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexImpl

foreign import drawIndexedWithInstanceCountAndFirstIndexAndFirstInstanceImpl
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit

drawIndexedWithInstanceCountAndFirstIndexAndFirstInstance
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndexAndFirstInstance =
  drawIndexedWithInstanceCountAndFirstIndexAndFirstInstanceImpl

foreign import drawIndexedWithFirstIndexAndBaseVertexAndFirstInstanceImpl
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit

drawIndexedWithFirstIndexAndBaseVertexAndFirstInstance
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithFirstIndexAndBaseVertexAndFirstInstance =
  drawIndexedWithFirstIndexAndBaseVertexAndFirstInstanceImpl

foreign import drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstanceImpl
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit

drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstance
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstance =
  drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstanceImpl

foreign import drawIndirectImpl
  :: GPURenderPassEncoder -> GPUBuffer -> GPUSize64 -> Effect Unit

drawIndirect :: GPURenderPassEncoder -> GPUBuffer -> GPUSize64 -> Effect Unit
drawIndirect = drawIndirectImpl

foreign import drawIndexedIndirectImpl
  :: GPURenderPassEncoder -> GPUBuffer -> GPUSize64 -> Effect Unit

drawIndexedIndirect
  :: GPURenderPassEncoder -> GPUBuffer -> GPUSize64 -> Effect Unit
drawIndexedIndirect = drawIndexedIndirectImpl

foreign import setBindGroupImpl
  :: GPURenderPassEncoder -> GPUIndex32 -> GPUBindGroup -> Effect Unit

setBindGroup
  :: GPURenderPassEncoder -> GPUIndex32 -> GPUBindGroup -> Effect Unit
setBindGroup = setBindGroupImpl

foreign import setBindGroupWithDynamicOffsetsImpl
  :: GPURenderPassEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Array GPUBufferDynamicOffset
  -> Effect Unit

setBindGroupWithDynamicOffsets
  :: GPURenderPassEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Array GPUBufferDynamicOffset
  -> Effect Unit
setBindGroupWithDynamicOffsets = setBindGroupWithDynamicOffsetsImpl

foreign import setBindGroupWithDyanmicOffsetBoundsImpl
  :: GPURenderPassEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Uint32Array
  -> GPUSize64
  -> GPUSize32
  -> Effect Unit

setBindGroupWithDyanmicOffsetBounds
  :: GPURenderPassEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Uint32Array
  -> GPUSize64
  -> GPUSize32
  -> Effect Unit
setBindGroupWithDyanmicOffsetBounds = setBindGroupWithDyanmicOffsetBoundsImpl

foreign import pushDebugGroupImpl
  :: GPURenderPassEncoder -> String -> Effect Unit

pushDebugGroup :: GPURenderPassEncoder -> String -> Effect Unit
pushDebugGroup = pushDebugGroupImpl

foreign import popDebugGroupImpl :: GPURenderPassEncoder -> Effect Unit

popDebugGroup :: GPURenderPassEncoder -> Effect Unit
popDebugGroup = popDebugGroupImpl

foreign import insertDebugMarkerImpl
  :: GPURenderPassEncoder -> String -> Effect Unit

insertDebugMarker :: GPURenderPassEncoder -> String -> Effect Unit
insertDebugMarker = insertDebugMarkerImpl
