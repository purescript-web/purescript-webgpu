-- @inline export setViewportImpl arity=7
-- @inline export setViewport arity=7
-- @inline export setScissorRectImpl arity=5
-- @inline export setScissorRect arity=5
-- @inline export setBlendConstantImpl arity=2
-- @inline export setBlendConstant arity=2
-- @inline export setStencilReferenceImpl arity=2
-- @inline export setStencilReference arity=2
-- @inline export beginOcclusionQueryImpl arity=2
-- @inline export beginOcclusionQuery arity=2
-- @inline export endOcclusionQueryImpl arity=1
-- @inline export endOcclusionQuery arity=1
-- @inline export executeBundlesImpl arity=2
-- @inline export executeBundles arity=2
-- @inline export endImpl arity=1
-- @inline export end arity=1
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
module Web.GPU.GPURenderPassEncoder
  ( GPURenderPassEncoder
  , beginOcclusionQuery
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
import Web.GPU.GPUBindGroup (GPUBindGroup)
import Web.GPU.GPUBuffer (GPUBuffer)
import Web.GPU.GPUColor (GPUColor)
import Web.GPU.GPUIndexFormat (GPUIndexFormat)
import Web.GPU.GPURenderBundle (GPURenderBundle)
import Web.GPU.GPURenderPipeline (GPURenderPipeline)
import Web.GPU.Internal.Types (GPUIndex32, GPUIntegerCoordinate, GPUSignedOffset32, GPUSize32, GPUSize64, GPUStencilValue, GPUBufferDynamicOffset)

data GPURenderPassEncoder

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