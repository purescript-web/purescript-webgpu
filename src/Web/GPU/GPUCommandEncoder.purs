-- @inline export beginRenderPass arity=2
-- @inline export beginComputePass arity=2
-- @inline export copyBufferToBuffer arity=6
-- @inline export copyTextureToBuffer arity=4
-- @inline export copyTextureToTexture arity=4
-- @inline export clearBuffer arity=2
-- @inline export clearBufferWithOffset arity=3
-- @inline export clearBufferWithSize arity=3
-- @inline export clearBufferWithOffsetAndSize arity=4
-- @inline export writeTimestamp arity=3
-- @inline export resolveQuerySet arity=6
-- @inline export finish arity=1
-- @inline export pushDebugGroup arity=2
-- @inline export popDebugGroup arity=1
-- @inline export insertDebugMarker arity=2
module Web.GPU.GPUCommandEncoder
  ( GPUCommandEncoder(..)
  , beginComputePass
  , beginRenderPass
  , clearBuffer
  , clearBufferWithOffset
  , clearBufferWithOffsetAndSize
  , clearBufferWithSize
  , copyBufferToBuffer
  , copyBufferToTexture
  , copyTextureToBuffer
  , copyTextureToTexture
  , finish
  , insertDebugMarker
  , popDebugGroup
  , pushDebugGroup
  , resolveQuerySet
  , writeTimestamp
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn5, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn5)
import Web.GPU.GPUBuffer (GPUBuffer)
import Web.GPU.GPUComputePassDescriptor (GPUComputePassDescriptor)
import Web.GPU.GPUComputePassEncoder (GPUComputePassEncoder)
import Web.GPU.GPUExtent3D (GPUExtent3D)
import Web.GPU.GPUImageCopyBuffer (GPUImageCopyBuffer)
import Web.GPU.GPUImageCopyTexture (GPUImageCopyTexture)
import Web.GPU.GPUQuerySet (GPUQuerySet)
import Web.GPU.GPURenderPassDescriptor (GPURenderPassDescriptor)
import Web.GPU.GPURenderPassEncoder (GPURenderPassEncoder)
import Web.GPU.Internal.GPUCommandBuffer (GPUCommandBuffer)
import Web.GPU.Internal.Types (GPUSize32, GPUSize64)

data GPUCommandEncoder

foreign import beginRenderPassImpl
  :: GPUCommandEncoder
  -> EffectFn1 GPURenderPassDescriptor GPURenderPassEncoder

beginRenderPass
  :: GPUCommandEncoder -> GPURenderPassDescriptor -> Effect GPURenderPassEncoder
beginRenderPass a b = runEffectFn1 (beginRenderPassImpl a) b

foreign import beginComputePassImpl
  :: GPUCommandEncoder
  -> EffectFn1 GPUComputePassDescriptor GPUComputePassEncoder

beginComputePass
  :: GPUCommandEncoder
  -> GPUComputePassDescriptor
  -> Effect GPUComputePassEncoder
beginComputePass a b = runEffectFn1 (beginComputePassImpl a) b

foreign import copyBufferToBufferImpl
  :: GPUCommandEncoder
  -> EffectFn5 GPUBuffer GPUSize64 GPUBuffer GPUSize64 GPUSize64 Unit

copyBufferToBuffer
  :: GPUCommandEncoder
  -> GPUBuffer
  -> Int
  -> GPUBuffer
  -> Int
  -> Int
  -> Effect Unit
copyBufferToBuffer a b c d e f = runEffectFn5 (copyBufferToBufferImpl a) b c d e
  f

foreign import copyBufferToTextureImpl
  :: GPUCommandEncoder
  -> EffectFn3 GPUImageCopyBuffer GPUImageCopyTexture GPUExtent3D Unit

copyBufferToTexture
  ∷ GPUCommandEncoder
  → GPUImageCopyBuffer
  → GPUImageCopyTexture
  → GPUExtent3D
  → Effect Unit
copyBufferToTexture a b c d = runEffectFn3 (copyBufferToTextureImpl a) b c d

foreign import copyTextureToBufferImpl
  :: GPUCommandEncoder
  -> EffectFn3 GPUImageCopyTexture GPUImageCopyBuffer GPUExtent3D Unit

copyTextureToBuffer
  :: GPUCommandEncoder
  -> GPUImageCopyTexture
  -> GPUImageCopyBuffer
  -> GPUExtent3D
  -> Effect Unit
copyTextureToBuffer a b c d = runEffectFn3 (copyTextureToBufferImpl a) b c d

foreign import copyTextureToTextureImpl
  :: GPUCommandEncoder
  -> EffectFn3 GPUImageCopyTexture GPUImageCopyTexture GPUExtent3D Unit

copyTextureToTexture
  :: GPUCommandEncoder
  -> GPUImageCopyTexture
  -> GPUImageCopyTexture
  -> GPUExtent3D
  -> Effect Unit
copyTextureToTexture a b c d = runEffectFn3 (copyTextureToTextureImpl a) b c d

foreign import clearBufferImpl
  :: GPUCommandEncoder
  -> EffectFn1 GPUBuffer Unit

clearBuffer :: GPUCommandEncoder -> GPUBuffer -> Effect Unit
clearBuffer a b = runEffectFn1 (clearBufferImpl a) b

foreign import clearBufferWithOffsetImpl
  :: GPUCommandEncoder
  -> EffectFn2 GPUBuffer GPUSize64 Unit

clearBufferWithOffset :: GPUCommandEncoder -> GPUBuffer -> Int -> Effect Unit
clearBufferWithOffset a b c = runEffectFn2 (clearBufferWithOffsetImpl a) b c

foreign import clearBufferWithSizeImpl
  :: GPUCommandEncoder
  ->EffectFn2 GPUBuffer  GPUSize64  Unit

clearBufferWithSize :: GPUCommandEncoder -> GPUBuffer -> Int -> Effect Unit
clearBufferWithSize a b c = runEffectFn2 (clearBufferWithSizeImpl a) b c

foreign import clearBufferWithOffsetAndSizeImpl
  :: GPUCommandEncoder
  -> EffectFn3 GPUBuffer GPUSize64 GPUSize64 Unit

clearBufferWithOffsetAndSize
  :: GPUCommandEncoder -> GPUBuffer -> Int -> Int -> Effect Unit
clearBufferWithOffsetAndSize a b c d = runEffectFn3 (clearBufferWithOffsetAndSizeImpl a) b c d

foreign import writeTimestampImpl
  :: GPUCommandEncoder -> EffectFn2 GPUQuerySet GPUSize32 Unit

writeTimestamp :: GPUCommandEncoder -> GPUQuerySet -> Int -> Effect Unit
writeTimestamp a b c = runEffectFn2 (writeTimestampImpl a) b c

foreign import resolveQuerySetImpl
  :: GPUCommandEncoder
  -> EffectFn5 GPUQuerySet GPUSize32 GPUSize32 GPUBuffer GPUSize64 Unit

resolveQuerySet
  :: GPUCommandEncoder
  -> GPUQuerySet
  -> Int
  -> Int
  -> GPUBuffer
  -> Int
  -> Effect Unit
resolveQuerySet a b c d e f = runEffectFn5 (resolveQuerySetImpl a) b c d e f

foreign import finishImpl :: GPUCommandEncoder -> Effect GPUCommandBuffer

finish :: GPUCommandEncoder -> Effect GPUCommandBuffer
finish = finishImpl

foreign import pushDebugGroupImpl ::  GPUCommandEncoder -> EffectFn1 String Unit

pushDebugGroup :: GPUCommandEncoder -> String -> Effect Unit
pushDebugGroup a b = runEffectFn1 (pushDebugGroupImpl a) b

foreign import popDebugGroupImpl :: GPUCommandEncoder -> Effect Unit

popDebugGroup :: GPUCommandEncoder -> Effect Unit
popDebugGroup = popDebugGroupImpl

foreign import insertDebugMarkerImpl
  :: GPUCommandEncoder -> EffectFn1 String Unit

insertDebugMarker :: GPUCommandEncoder -> String -> Effect Unit
insertDebugMarker a b = runEffectFn1 (insertDebugMarkerImpl a) b