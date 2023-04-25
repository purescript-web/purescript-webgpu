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
module Web.GPU.GPURenderBundleEncoder
  ( GPURenderBundleEncoder
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
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, EffectFn6, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5, runEffectFn6)
import Web.GPU.GPUBindGroup (GPUBindGroup)
import Web.GPU.GPUBuffer (GPUBuffer)
import Web.GPU.GPUIndexFormat (GPUIndexFormat)
import Web.GPU.GPURenderBundle (GPURenderBundle)
import Web.GPU.GPURenderBundleDescriptor (GPURenderBundleDescriptor)
import Web.GPU.GPURenderPipeline (GPURenderPipeline)
import Web.GPU.Internal.Types (GPUBufferDynamicOffset, GPUIndex32, GPUSignedOffset32, GPUSize32, GPUSize64)

data GPURenderBundleEncoder

foreign import finishImpl :: EffectFn2 GPURenderBundleEncoder GPURenderBundleDescriptor  GPURenderBundle
finish
  ∷ GPURenderBundleEncoder -> GPURenderBundleDescriptor → Effect GPURenderBundle
finish a b = runEffectFn2 finishImpl a b

foreign import setPipelineImpl :: EffectFn2 GPURenderBundleEncoder GPURenderPipeline  Unit
setPipeline :: GPURenderBundleEncoder -> GPURenderPipeline -> Effect Unit
setPipeline a b = runEffectFn2 setPipelineImpl a b

foreign import setIndexBufferImpl :: EffectFn3 GPURenderBundleEncoder GPUBuffer GPUIndexFormat  Unit
setIndexBuffer
  :: GPURenderBundleEncoder -> GPUBuffer -> GPUIndexFormat -> Effect Unit
setIndexBuffer a b c = runEffectFn3 setIndexBufferImpl a b c

foreign import setIndexBufferWithSizeImpl :: EffectFn4 GPURenderBundleEncoder GPUBuffer GPUIndexFormat GPUSize64  Unit
setIndexBufferWithSize
  :: GPURenderBundleEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> Effect Unit
setIndexBufferWithSize a b c d = runEffectFn4 setIndexBufferWithSizeImpl a b c d

foreign import setIndexBufferWithOffsetImpl :: EffectFn4 GPURenderBundleEncoder GPUBuffer GPUIndexFormat GPUSize64  Unit
setIndexBufferWithOffset
  :: GPURenderBundleEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> Effect Unit
setIndexBufferWithOffset a b c d = runEffectFn4 setIndexBufferWithOffsetImpl a b c d

foreign import setIndexBufferWithOffsetAndSizeImpl :: EffectFn5 GPURenderBundleEncoder GPUBuffer GPUIndexFormat GPUSize64 GPUSize64  Unit
setIndexBufferWithOffsetAndSize
  :: GPURenderBundleEncoder
  -> GPUBuffer
  -> GPUIndexFormat
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit
setIndexBufferWithOffsetAndSize a b c d e = runEffectFn5 setIndexBufferWithOffsetAndSizeImpl a b c d e

foreign import setVertexBufferImpl :: EffectFn3 GPURenderBundleEncoder GPUIndex32 GPUBuffer  Unit
setVertexBuffer
  :: GPURenderBundleEncoder -> GPUIndex32 -> GPUBuffer -> Effect Unit
setVertexBuffer a b c = runEffectFn3 setVertexBufferImpl a b c

foreign import setVertexBufferWithOffsetImpl :: EffectFn4 GPURenderBundleEncoder GPUIndex32 GPUBuffer GPUSize64  Unit
setVertexBufferWithOffset
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBuffer
  -> GPUSize64
  -> Effect Unit
setVertexBufferWithOffset a b c d = runEffectFn4 setVertexBufferWithOffsetImpl a b c d

foreign import setVertexBufferWithSizeImpl :: EffectFn4 GPURenderBundleEncoder GPUIndex32 GPUBuffer GPUSize64  Unit
setVertexBufferWithSize
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBuffer
  -> GPUSize64
  -> Effect Unit
setVertexBufferWithSize a b c d = runEffectFn4 setVertexBufferWithSizeImpl a b c d

foreign import setVertexBufferWithOffsetAndSizeImpl :: EffectFn5 GPURenderBundleEncoder GPUIndex32 GPUBuffer GPUSize64 GPUSize64  Unit
setVertexBufferWithOffsetAndSize
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBuffer
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit
setVertexBufferWithOffsetAndSize a b c d e = runEffectFn5 setVertexBufferWithOffsetAndSizeImpl a b c d e

foreign import drawImpl :: EffectFn2 GPURenderBundleEncoder GPUSize32  Unit
draw :: GPURenderBundleEncoder -> GPUSize32 -> Effect Unit
draw a b = runEffectFn2 drawImpl a b

foreign import drawWithInstanceCountImpl :: EffectFn3 GPURenderBundleEncoder GPUSize32 GPUSize32  Unit
drawWithInstanceCount
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawWithInstanceCount a b c = runEffectFn3 drawWithInstanceCountImpl a b c

foreign import drawWithFirstVertexImpl :: EffectFn2 GPURenderBundleEncoder GPUSize32  Unit
drawWithFirstVertex
  :: GPURenderBundleEncoder -> GPUSize32 -> Effect Unit
drawWithFirstVertex a b = runEffectFn2 drawWithFirstVertexImpl a b

foreign import drawWithFirstInstanceImpl :: EffectFn2 GPURenderBundleEncoder GPUSize32  Unit
drawWithFirstInstance
  :: GPURenderBundleEncoder -> GPUSize32 -> Effect Unit
drawWithFirstInstance a b = runEffectFn2 drawWithFirstInstanceImpl a b

foreign import drawWithInstanceCountAndFirstVertexImpl :: EffectFn4 GPURenderBundleEncoder GPUSize32 GPUSize32 GPUSize32  Unit
drawWithInstanceCountAndFirstVertex
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawWithInstanceCountAndFirstVertex a b c d = runEffectFn4 drawWithInstanceCountAndFirstVertexImpl a b c d

foreign import drawWithInstanceCountAndFirstInstanceImpl :: EffectFn4 GPURenderBundleEncoder GPUSize32 GPUSize32 GPUSize32  Unit
drawWithInstanceCountAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawWithInstanceCountAndFirstInstance a b c d =
  runEffectFn4 drawWithInstanceCountAndFirstInstanceImpl a b c d

foreign import drawWithFirstVertexAndFirstInstanceImpl :: EffectFn4 GPURenderBundleEncoder GPUSize32 GPUSize32 GPUSize32  Unit
drawWithFirstVertexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawWithFirstVertexAndFirstInstance a b c d = runEffectFn4 drawWithFirstVertexAndFirstInstanceImpl a b c d

foreign import drawWithInstanceCountAndFirstVertexAndFirstInstanceImpl :: EffectFn5 GPURenderBundleEncoder GPUSize32 GPUSize32 GPUSize32 GPUSize32  Unit
drawWithInstanceCountAndFirstVertexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawWithInstanceCountAndFirstVertexAndFirstInstance a b c d e =
  runEffectFn5 drawWithInstanceCountAndFirstVertexAndFirstInstanceImpl a b c d e

foreign import drawIndexedImpl :: EffectFn2 GPURenderBundleEncoder GPUSize32  Unit
drawIndexed :: GPURenderBundleEncoder -> GPUSize32 -> Effect Unit
drawIndexed a b = runEffectFn2 drawIndexedImpl a b

foreign import drawIndexedWithInstanceCountImpl :: EffectFn3 GPURenderBundleEncoder GPUSize32 GPUSize32  Unit
drawIndexedWithInstanceCount
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithInstanceCount a b c = runEffectFn3 drawIndexedWithInstanceCountImpl a b c

foreign import drawIndexedWithFirstIndexImpl :: EffectFn3 GPURenderBundleEncoder GPUSize32 GPUSize32  Unit
drawIndexedWithFirstIndex
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithFirstIndex a b c = runEffectFn3 drawIndexedWithFirstIndexImpl a b c

foreign import drawIndexedWithBaseVertexImpl :: EffectFn3 GPURenderBundleEncoder GPUSize32 GPUSignedOffset32  Unit
drawIndexedWithBaseVertex
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSignedOffset32 -> Effect Unit
drawIndexedWithBaseVertex a b c = runEffectFn3 drawIndexedWithBaseVertexImpl a b c

foreign import drawIndexedWithFirstInstanceImpl :: EffectFn3 GPURenderBundleEncoder GPUSize32 GPUSize32  Unit
drawIndexedWithFirstInstance
  :: GPURenderBundleEncoder -> GPUSize32 -> GPUSize32 -> Effect Unit
drawIndexedWithFirstInstance a b c = runEffectFn3 drawIndexedWithFirstInstanceImpl a b c

foreign import drawIndexedWithInstanceCountAndFirstIndexImpl :: EffectFn4 GPURenderBundleEncoder GPUSize32 GPUSize32 GPUSize32  Unit
drawIndexedWithInstanceCountAndFirstIndex
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndex a b c d =
  runEffectFn4 drawIndexedWithInstanceCountAndFirstIndexImpl a b c d

foreign import drawIndexedWithInstanceCountAndBaseVertexImpl :: EffectFn4 GPURenderBundleEncoder GPUSize32 GPUSize32 GPUSignedOffset32  Unit
drawIndexedWithInstanceCountAndBaseVertex
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit
drawIndexedWithInstanceCountAndBaseVertex a b c d =
  runEffectFn4 drawIndexedWithInstanceCountAndBaseVertexImpl a b c d

foreign import drawIndexedWithInstanceCountAndFirstInstanceImpl :: EffectFn4 GPURenderBundleEncoder GPUSize32 GPUSize32 GPUSize32  Unit
drawIndexedWithInstanceCountAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstInstance a b c d =
  runEffectFn4 drawIndexedWithInstanceCountAndFirstInstanceImpl a b c d

foreign import drawIndexedWithFirstIndexAndBaseVertexImpl :: EffectFn4 GPURenderBundleEncoder GPUSize32 GPUSize32 GPUSignedOffset32  Unit
drawIndexedWithFirstIndexAndBaseVertex
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit
drawIndexedWithFirstIndexAndBaseVertex a b c d =
  runEffectFn4 drawIndexedWithFirstIndexAndBaseVertexImpl a b c d

foreign import drawIndexedWithFirstIndexAndFirstInstanceImpl :: EffectFn4 GPURenderBundleEncoder GPUSize32 GPUSize32 GPUSize32  Unit
drawIndexedWithFirstIndexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithFirstIndexAndFirstInstance a b c d =
  runEffectFn4 drawIndexedWithFirstIndexAndFirstInstanceImpl a b c d

foreign import drawIndexedWithBaseVertexAndFirstInstanceImpl :: EffectFn4 GPURenderBundleEncoder GPUSize32 GPUSignedOffset32 GPUSize32  Unit
drawIndexedWithBaseVertexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithBaseVertexAndFirstInstance a b c d =
  runEffectFn4 drawIndexedWithBaseVertexAndFirstInstanceImpl a b c d

foreign import drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexImpl :: EffectFn5 GPURenderBundleEncoder GPUSize32 GPUSize32 GPUSize32 GPUSignedOffset32  Unit
drawIndexedWithInstanceCountAndFirstIndexAndBaseVertex
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndexAndBaseVertex a b c d e =
  runEffectFn5 drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexImpl a b c d e

foreign import drawIndexedWithInstanceCountAndFirstIndexAndFirstInstanceImpl :: EffectFn5 GPURenderBundleEncoder GPUSize32 GPUSize32 GPUSize32 GPUSize32  Unit
drawIndexedWithInstanceCountAndFirstIndexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndexAndFirstInstance a b c d e = runEffectFn5
  drawIndexedWithInstanceCountAndFirstIndexAndFirstInstanceImpl a b c d e

foreign import drawIndexedWithFirstIndexAndBaseVertexAndFirstInstanceImpl :: EffectFn5 GPURenderBundleEncoder GPUSize32 GPUSize32 GPUSignedOffset32 GPUSize32  Unit
drawIndexedWithFirstIndexAndBaseVertexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithFirstIndexAndBaseVertexAndFirstInstance a b c d e =
  runEffectFn5 drawIndexedWithFirstIndexAndBaseVertexAndFirstInstanceImpl a b c d e

foreign import drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstanceImpl :: EffectFn6 GPURenderBundleEncoder GPUSize32 GPUSize32 GPUSize32 GPUSignedOffset32 GPUSize32  Unit
drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstance
  :: GPURenderBundleEncoder
  -> GPUSize32
  -> GPUSize32
  -> GPUSize32
  -> GPUSignedOffset32
  -> GPUSize32
  -> Effect Unit
drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstance a b c d e f =
  runEffectFn6 drawIndexedWithInstanceCountAndFirstIndexAndBaseVertexAndFirstInstanceImpl a b c d e f

foreign import drawIndirectImpl :: EffectFn3 GPURenderBundleEncoder GPUBuffer GPUSize64  Unit
drawIndirect :: GPURenderBundleEncoder -> GPUBuffer -> GPUSize64 -> Effect Unit
drawIndirect a b c = runEffectFn3 drawIndirectImpl a b c

foreign import drawIndexedIndirectImpl :: EffectFn3 GPURenderBundleEncoder GPUBuffer GPUSize64  Unit
drawIndexedIndirect
  :: GPURenderBundleEncoder -> GPUBuffer -> GPUSize64 -> Effect Unit
drawIndexedIndirect a b c = runEffectFn3 drawIndexedIndirectImpl a b c

foreign import setBindGroupImpl :: EffectFn3 GPURenderBundleEncoder GPUIndex32 GPUBindGroup  Unit
setBindGroup
  :: GPURenderBundleEncoder -> GPUIndex32 -> GPUBindGroup -> Effect Unit
setBindGroup a b c = runEffectFn3 setBindGroupImpl a b c

foreign import setBindGroupWithDynamicOffsetsImpl :: EffectFn4 GPURenderBundleEncoder GPUIndex32 GPUBindGroup (Array GPUBufferDynamicOffset)  Unit
setBindGroupWithDynamicOffsets
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Array GPUBufferDynamicOffset
  -> Effect Unit
setBindGroupWithDynamicOffsets a b c d = runEffectFn4 setBindGroupWithDynamicOffsetsImpl a b c d

foreign import setBindGroupWithDyanmicOffsetBoundsImpl :: EffectFn6 GPURenderBundleEncoder GPUIndex32 GPUBindGroup Uint32Array GPUSize64 GPUSize32  Unit
setBindGroupWithDyanmicOffsetBounds
  :: GPURenderBundleEncoder
  -> GPUIndex32
  -> GPUBindGroup
  -> Uint32Array
  -> GPUSize64
  -> GPUSize32
  -> Effect Unit
setBindGroupWithDyanmicOffsetBounds a b c d e f = runEffectFn6 setBindGroupWithDyanmicOffsetBoundsImpl a b c d e f

foreign import pushDebugGroupImpl :: EffectFn2 GPURenderBundleEncoder String  Unit
pushDebugGroup :: GPURenderBundleEncoder -> String -> Effect Unit
pushDebugGroup a b = runEffectFn2 pushDebugGroupImpl a b

foreign import popDebugGroupImpl :: EffectFn1 GPURenderBundleEncoder  Unit
popDebugGroup :: GPURenderBundleEncoder -> Effect Unit
popDebugGroup a = runEffectFn1 popDebugGroupImpl a

foreign import insertDebugMarkerImpl :: EffectFn2 GPURenderBundleEncoder String  Unit
insertDebugMarker :: GPURenderBundleEncoder -> String -> Effect Unit
insertDebugMarker a b = runEffectFn2 insertDebugMarkerImpl a b