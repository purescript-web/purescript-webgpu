-- @inline export setViewport arity=7
-- @inline export setScissorRect arity=5
-- @inline export setBlendConstant arity=2
-- @inline export setStencilReference arity=2
-- @inline export beginOcclusionQuery arity=2
-- @inline export endOcclusionQuery arity=1
-- @inline export executeBundles arity=2
-- @inline export end arity=1
-- @inline export setPipeline arity=2
-- @inline export setIndexBuffer arity=3
-- @inline export setIndexBufferWithSize arity=4
-- @inline export setIndexBufferWithOffset arity=4
-- @inline export setIndexBufferWithOffsetAndSize arity=5
-- @inline export setVertexBuffer arity=3
-- @inline export setVertexBufferWithOffset arity=4
-- @inline export setVertexBufferWithSize arity=4
-- @inline export setVertexBufferWithOffsetAndSize arity=5
-- @inline export draw arity=2
-- @inline export drawWithInstanceCount arity=3
-- @inline export drawWithFirstVertex arity=2
-- @inline export drawWithFirstInstance arity=2
-- @inline export drawWithInstanceCountAndFirstVertex arity=4
-- @inline export drawWithInstanceCountAndFirstInstance arity=4
-- @inline export drawWithFirstVertexAndFirstInstance arity=4
-- @inline export drawWithInstanceCountAndFirstVertexAndFirstInstance arity=5
-- @inline export drawIndexed arity=2
-- @inline export drawIndexedWithInstanceCount arity=3
-- @inline export drawIndexedWithFirstIndex arity=3
-- @inline export drawIndexedWithBaseVertex arity=3
-- @inline export drawIndexedWithFirstInstance arity=3
-- @inline export drawIndexedWithInstanceCountAndFirstIndex arity=4
-- @inline export drawIndexedWithInstanceCountAndBaseVertex arity=4
-- @inline export drawIndexedWithInstanceCountAndFirstInstance arity=4
-- @inline export drawIndexedWithFirstIndexAndBaseVertex arity=4
-- @inline export drawIndexedWithFirstIndexAndFirstInstance arity=4
-- @inline export drawIndexedWithBaseVertexAndFirstInstance arity=4
-- @inline export drawIndexedWithInstanceCountAndFirstIndexAndBaseVertex arity=5
-- @inline export drawIndexedWithInstanceCountAndFirstIndexAndFirstInstance arity=5
-- @inline export drawIndexedWithFirstIndexAndBaseVertexAndFirstInstance arity=5
-- @inline export drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstance arity=6
-- @inline export drawIndirect arity=3
-- @inline export drawIndexedIndirect arity=3
-- @inline export setBindGroup arity=3
-- @inline export setBindGroupWithDynamicOffsets arity=4
-- @inline export setBindGroupWithDyanmicOffsetBounds arity=6
-- @inline export pushDebugGroup arity=2
-- @inline export popDebugGroup arity=1
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
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, EffectFn6, EffectFn7, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5, runEffectFn6, runEffectFn7)
import Web.GPU.GPUBindGroup (GPUBindGroup)
import Web.GPU.GPUBuffer (GPUBuffer)
import Web.GPU.GPUColor (GPUColor)
import Web.GPU.GPUIndexFormat (GPUIndexFormat)
import Web.GPU.GPURenderBundle (GPURenderBundle)
import Web.GPU.GPURenderPipeline (GPURenderPipeline)
import Web.GPU.Internal.Types (GPUIndex32, GPUIntegerCoordinate, GPUSignedOffset32, GPUSize32, GPUSize64, GPUStencilValue, GPUBufferDynamicOffset)

data GPURenderPassEncoder

foreign import setViewportImpl :: EffectFn7 GPURenderPassEncoder Number Number Number Number Number Number  Unit
setViewport
  :: GPURenderPassEncoder
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Effect Unit
setViewport a b c d e f g = runEffectFn7 setViewportImpl a b c d e f g

foreign import setScissorRectImpl :: EffectFn5 GPURenderPassEncoder GPUIntegerCoordinate GPUIntegerCoordinate GPUIntegerCoordinate GPUIntegerCoordinate  Unit
setScissorRect
  :: GPURenderPassEncoder
  -> GPUIntegerCoordinate
  -> GPUIntegerCoordinate
  -> GPUIntegerCoordinate
  -> GPUIntegerCoordinate
  -> Effect Unit
setScissorRect a b c d e = runEffectFn5 setScissorRectImpl a b c d e

foreign import setBlendConstantImpl :: EffectFn2 GPURenderPassEncoder GPUColor  Unit
setBlendConstant :: GPURenderPassEncoder -> GPUColor -> Effect Unit
setBlendConstant a b = runEffectFn2 setBlendConstantImpl a b

foreign import setStencilReferenceImpl :: EffectFn2 GPURenderPassEncoder GPUStencilValue  Unit
setStencilReference :: GPURenderPassEncoder -> GPUStencilValue -> Effect Unit
setStencilReference a b = runEffectFn2 setStencilReferenceImpl a b

foreign import beginOcclusionQueryImpl :: EffectFn2 GPURenderPassEncoder GPUSize32  Unit
beginOcclusionQuery :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit
beginOcclusionQuery a b = runEffectFn2 beginOcclusionQueryImpl a b

foreign import endOcclusionQueryImpl :: EffectFn1 GPURenderPassEncoder  Unit
endOcclusionQuery :: GPURenderPassEncoder -> Effect Unit
endOcclusionQuery a = runEffectFn1 endOcclusionQueryImpl a

foreign import executeBundlesImpl :: EffectFn2 GPURenderPassEncoder (Array GPURenderBundle)  Unit
executeBundles :: GPURenderPassEncoder -> Array GPURenderBundle -> Effect Unit
executeBundles a b = runEffectFn2 executeBundlesImpl a b

foreign import endImpl :: EffectFn1 GPURenderPassEncoder  Unit
end :: GPURenderPassEncoder -> Effect Unit
end a = runEffectFn1 endImpl a

foreign import setPipelineImpl :: EffectFn2 GPURenderPassEncoder GPURenderPipeline  Unit
setPipeline :: GPURenderPassEncoder -> GPURenderPipeline -> Effect Unit
setPipeline a b = runEffectFn2 setPipelineImpl a b

foreign import setIndexBufferImpl :: EffectFn3 GPURenderPassEncoder GPUBuffer GPUIndexFormat  Unit
setIndexBuffer
  :: GPURenderPassEncoder -> GPUBuffer -> GPUIndexFormat -> Effect Unit
setIndexBuffer a b c = runEffectFn3 setIndexBufferImpl a b c

foreign import setIndexBufferWithSizeImpl :: EffectFn4 GPURenderPassEncoder GPUBuffer GPUIndexFormat GPUSize64  Unit
setIndexBufferWithSize
  :: GPURenderPassEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> Effect Unit
setIndexBufferWithSize a b c d = runEffectFn4 setIndexBufferWithSizeImpl a b c d

foreign import setIndexBufferWithOffsetImpl :: EffectFn4 GPURenderPassEncoder GPUBuffer GPUIndexFormat GPUSize64  Unit
setIndexBufferWithOffset
  :: GPURenderPassEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> Effect Unit
setIndexBufferWithOffset a b c d = runEffectFn4 setIndexBufferWithOffsetImpl a b c d

foreign import setIndexBufferWithOffsetAndSizeImpl :: EffectFn5 GPURenderPassEncoder GPUBuffer GPUIndexFormat GPUSize64 GPUSize64  Unit
setIndexBufferWithOffsetAndSize
  :: GPURenderPassEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit
setIndexBufferWithOffsetAndSize a b c d e = runEffectFn5 setIndexBufferWithOffsetAndSizeImpl a b c d e

foreign import setVertexBufferImpl :: EffectFn3 GPURenderPassEncoder GPUIndex32 GPUBuffer  Unit
setVertexBuffer
  :: GPURenderPassEncoder -> GPUIndex32 -> GPUBuffer -> Effect Unit
setVertexBuffer a b c = runEffectFn3 setVertexBufferImpl a b c

foreign import setVertexBufferWithOffsetImpl :: EffectFn4 GPURenderPassEncoder GPUIndex32 GPUBuffer GPUSize64  Unit
setVertexBufferWithOffset
  :: GPURenderPassEncoder -> GPUIndex32 -> GPUBuffer -> GPUSize64 -> Effect Unit
setVertexBufferWithOffset a b c d = runEffectFn4 setVertexBufferWithOffsetImpl a b c d

foreign import setVertexBufferWithSizeImpl :: EffectFn4 GPURenderPassEncoder GPUIndex32 GPUBuffer GPUSize64  Unit
setVertexBufferWithSize
  :: GPURenderPassEncoder -> GPUIndex32 -> GPUBuffer -> GPUSize64 -> Effect Unit
setVertexBufferWithSize a b c d = runEffectFn4 setVertexBufferWithSizeImpl a b c d

foreign import setVertexBufferWithOffsetAndSizeImpl :: EffectFn5 GPURenderPassEncoder GPUIndex32 GPUBuffer GPUSize64 GPUSize64  Unit
setVertexBufferWithOffsetAndSize
  :: GPURenderPassEncoder
  -> GPUIndex32
  -> GPUBuffer
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit
setVertexBufferWithOffsetAndSize a b c d e = runEffectFn5 setVertexBufferWithOffsetAndSizeImpl a b c d e

foreign import drawImpl :: EffectFn2 GPURenderPassEncoder GPUSize32  Unit
draw :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit
draw a b = runEffectFn2 drawImpl a b

foreign import drawWithInstanceCountImpl :: EffectFn3 GPURenderPassEncoder GPUSize32 GPUSize32  Unit
drawWithInstanceCount
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawWithInstanceCount a b c = runEffectFn3 drawWithInstanceCountImpl a b c

foreign import drawWithFirstVertexImpl :: EffectFn2 GPURenderPassEncoder GPUSize32  Unit
drawWithFirstVertex
  :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit
drawWithFirstVertex a b = runEffectFn2 drawWithFirstVertexImpl a b

foreign import drawWithFirstInstanceImpl :: EffectFn2 GPURenderPassEncoder GPUSize32  Unit
drawWithFirstInstance
  :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit
drawWithFirstInstance a b = runEffectFn2 drawWithFirstInstanceImpl a b

foreign import drawWithInstanceCountAndFirstVertexImpl :: EffectFn4 GPURenderPassEncoder GPUSize32 GPUSize32 GPUSize32  Unit
drawWithInstanceCountAndFirstVertex
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit
drawWithInstanceCountAndFirstVertex a b c d = runEffectFn4 drawWithInstanceCountAndFirstVertexImpl a b c d

foreign import drawWithInstanceCountAndFirstInstanceImpl :: EffectFn4 GPURenderPassEncoder GPUSize32 GPUSize32 GPUSize32  Unit
drawWithInstanceCountAndFirstInstance
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit
drawWithInstanceCountAndFirstInstance a b c d =
  runEffectFn4 drawWithInstanceCountAndFirstInstanceImpl a b c d

foreign import drawWithFirstVertexAndFirstInstanceImpl :: EffectFn4 GPURenderPassEncoder GPUSize32 GPUSize32 GPUSize32  Unit
drawWithFirstVertexAndFirstInstance
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit
drawWithFirstVertexAndFirstInstance a b c d = runEffectFn4 drawWithFirstVertexAndFirstInstanceImpl a b c d

foreign import drawWithInstanceCountAndFirstVertexAndFirstInstanceImpl :: EffectFn5 GPURenderPassEncoder GPUSize32 GPUSize32 GPUSize32 GPUSize32  Unit
drawWithInstanceCountAndFirstVertexAndFirstInstance
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawWithInstanceCountAndFirstVertexAndFirstInstance a b c d e =
  runEffectFn5 drawWithInstanceCountAndFirstVertexAndFirstInstanceImpl a b c d e

foreign import drawIndexedImpl :: EffectFn2 GPURenderPassEncoder GPUSize32  Unit
drawIndexed :: GPURenderPassEncoder -> GPUSize32 -> Effect Unit
drawIndexed a b = runEffectFn2 drawIndexedImpl a b

foreign import drawIndexedWithInstanceCountImpl :: EffectFn3 GPURenderPassEncoder GPUSize32 GPUSize32  Unit
drawIndexedWithInstanceCount
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithInstanceCount a b c = runEffectFn3 drawIndexedWithInstanceCountImpl a b c

foreign import drawIndexedWithFirstIndexImpl :: EffectFn3 GPURenderPassEncoder GPUSize32 GPUSize32  Unit
drawIndexedWithFirstIndex
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithFirstIndex a b c = runEffectFn3 drawIndexedWithFirstIndexImpl a b c

foreign import drawIndexedWithBaseVertexImpl :: EffectFn3 GPURenderPassEncoder GPUSize32 GPUSignedOffset32  Unit
drawIndexedWithBaseVertex
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSignedOffset32 -> Effect Unit
drawIndexedWithBaseVertex a b c = runEffectFn3 drawIndexedWithBaseVertexImpl a b c

foreign import drawIndexedWithFirstInstanceImpl :: EffectFn3 GPURenderPassEncoder GPUSize32 GPUSize32  Unit
drawIndexedWithFirstInstance
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithFirstInstance a b c = runEffectFn3 drawIndexedWithFirstInstanceImpl a b c

foreign import drawIndexedWithInstanceCountAndFirstIndexImpl :: EffectFn4 GPURenderPassEncoder GPUSize32 GPUSize32 GPUSize32  Unit
drawIndexedWithInstanceCountAndFirstIndex
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndex a b c d =
  runEffectFn4 drawIndexedWithInstanceCountAndFirstIndexImpl a b c d

foreign import drawIndexedWithInstanceCountAndBaseVertexImpl :: EffectFn4 GPURenderPassEncoder GPUSize32 GPUSize32 GPUSignedOffset32  Unit
drawIndexedWithInstanceCountAndBaseVertex
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit
drawIndexedWithInstanceCountAndBaseVertex a b c d =
  runEffectFn4 drawIndexedWithInstanceCountAndBaseVertexImpl a b c d

foreign import drawIndexedWithInstanceCountAndFirstInstanceImpl :: EffectFn4 GPURenderPassEncoder GPUSize32 GPUSize32 GPUSize32  Unit
drawIndexedWithInstanceCountAndFirstInstance
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithInstanceCountAndFirstInstance a b c d =
  runEffectFn4 drawIndexedWithInstanceCountAndFirstInstanceImpl a b c d

foreign import drawIndexedWithFirstIndexAndBaseVertexImpl :: EffectFn4 GPURenderPassEncoder GPUSize32 GPUSize32 GPUSignedOffset32  Unit
drawIndexedWithFirstIndexAndBaseVertex
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit
drawIndexedWithFirstIndexAndBaseVertex a b c d = runEffectFn4
  drawIndexedWithFirstIndexAndBaseVertexImpl a b c d

foreign import drawIndexedWithFirstIndexAndFirstInstanceImpl :: EffectFn4 GPURenderPassEncoder GPUSize32 GPUSize32 GPUSize32  Unit
drawIndexedWithFirstIndexAndFirstInstance
  :: GPURenderPassEncoder -> GPUSize32 -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithFirstIndexAndFirstInstance a b c d =
  runEffectFn4 drawIndexedWithFirstIndexAndFirstInstanceImpl a b c d

foreign import drawIndexedWithBaseVertexAndFirstInstanceImpl :: EffectFn4 GPURenderPassEncoder GPUSize32 GPUSignedOffset32 GPUSize32  Unit
drawIndexedWithBaseVertexAndFirstInstance
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithBaseVertexAndFirstInstance a b c d =
  runEffectFn4 drawIndexedWithBaseVertexAndFirstInstanceImpl a b c d

foreign import drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexImpl :: EffectFn5 GPURenderPassEncoder GPUSize32 GPUSize32 GPUSize32 GPUSignedOffset32  Unit
drawIndexedWithInstanceCountAndFirstIndexAndBaseVertex
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndexAndBaseVertex a b c d e =
  runEffectFn5 drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexImpl a b c d e

foreign import drawIndexedWithInstanceCountAndFirstIndexAndFirstInstanceImpl :: EffectFn5 GPURenderPassEncoder GPUSize32 GPUSize32 GPUSize32 GPUSize32  Unit
drawIndexedWithInstanceCountAndFirstIndexAndFirstInstance
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndexAndFirstInstance a b c d e =
  runEffectFn5 drawIndexedWithInstanceCountAndFirstIndexAndFirstInstanceImpl a b c d e

foreign import drawIndexedWithFirstIndexAndBaseVertexAndFirstInstanceImpl :: EffectFn5 GPURenderPassEncoder GPUSize32 GPUSize32 GPUSignedOffset32 GPUSize32  Unit
drawIndexedWithFirstIndexAndBaseVertexAndFirstInstance
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithFirstIndexAndBaseVertexAndFirstInstance a b c d e =
  runEffectFn5 drawIndexedWithFirstIndexAndBaseVertexAndFirstInstanceImpl a b c d e

foreign import drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstanceImpl :: EffectFn6 GPURenderPassEncoder GPUSize32 GPUSize32 GPUSize32 GPUSignedOffset32 GPUSize32  Unit
drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstance
  :: GPURenderPassEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstance a b c d e f =
  runEffectFn6 drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstanceImpl a b c d e f

foreign import drawIndirectImpl :: EffectFn3 GPURenderPassEncoder GPUBuffer GPUSize64  Unit
drawIndirect :: GPURenderPassEncoder -> GPUBuffer -> GPUSize64 -> Effect Unit
drawIndirect a b c = runEffectFn3 drawIndirectImpl a b c

foreign import drawIndexedIndirectImpl :: EffectFn3 GPURenderPassEncoder GPUBuffer GPUSize64  Unit
drawIndexedIndirect
  :: GPURenderPassEncoder -> GPUBuffer -> GPUSize64 -> Effect Unit
drawIndexedIndirect a b c = runEffectFn3 drawIndexedIndirectImpl a b c

foreign import setBindGroupImpl :: EffectFn3 GPURenderPassEncoder GPUIndex32 GPUBindGroup  Unit
setBindGroup
  :: GPURenderPassEncoder -> GPUIndex32 -> GPUBindGroup -> Effect Unit
setBindGroup a b c = runEffectFn3 setBindGroupImpl a b c

foreign import setBindGroupWithDynamicOffsetsImpl :: EffectFn4 GPURenderPassEncoder GPUIndex32 GPUBindGroup (Array GPUBufferDynamicOffset)  Unit
setBindGroupWithDynamicOffsets
  :: GPURenderPassEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Array GPUBufferDynamicOffset
  -> Effect Unit
setBindGroupWithDynamicOffsets a b c d = runEffectFn4 setBindGroupWithDynamicOffsetsImpl a b c d

foreign import setBindGroupWithDyanmicOffsetBoundsImpl :: EffectFn6 GPURenderPassEncoder GPUIndex32 GPUBindGroup Uint32Array GPUSize64 GPUSize32  Unit
setBindGroupWithDyanmicOffsetBounds
  :: GPURenderPassEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Uint32Array
  -> GPUSize64
  -> GPUSize32
  -> Effect Unit
setBindGroupWithDyanmicOffsetBounds a b c d e f = runEffectFn6 setBindGroupWithDyanmicOffsetBoundsImpl a b c d e f

foreign import pushDebugGroupImpl :: EffectFn2 GPURenderPassEncoder String  Unit
pushDebugGroup :: GPURenderPassEncoder -> String -> Effect Unit
pushDebugGroup a b = runEffectFn2 pushDebugGroupImpl a b

foreign import popDebugGroupImpl :: EffectFn1 GPURenderPassEncoder  Unit
popDebugGroup :: GPURenderPassEncoder -> Effect Unit
popDebugGroup a = runEffectFn1 popDebugGroupImpl a

foreign import insertDebugMarkerImpl :: EffectFn2 GPURenderPassEncoder String  Unit
insertDebugMarker :: GPURenderPassEncoder -> String -> Effect Unit
insertDebugMarker a b = runEffectFn2 insertDebugMarkerImpl a b