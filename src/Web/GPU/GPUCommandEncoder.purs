-- @inline export beginRenderPassImpl arity=2
-- @inline export beginRenderPass arity=2
-- @inline export beginComputePassImpl arity=2
-- @inline export beginComputePass arity=2
-- @inline export copyBufferToBufferImpl arity=6
-- @inline export copyBufferToBuffer arity=6
-- @inline export copyBufferToTextureImpl arity=4
-- @inline export copyTextureToBufferImpl arity=4
-- @inline export copyTextureToBuffer arity=4
-- @inline export copyTextureToTextureImpl arity=4
-- @inline export copyTextureToTexture arity=4
-- @inline export clearBufferImpl arity=2
-- @inline export clearBuffer arity=2
-- @inline export clearBufferWithOffsetImpl arity=3
-- @inline export clearBufferWithOffset arity=3
-- @inline export clearBufferWithSizeImpl arity=3
-- @inline export clearBufferWithSize arity=3
-- @inline export clearBufferWithOffsetAndSizeImpl arity=4
-- @inline export clearBufferWithOffsetAndSize arity=4
-- @inline export writeTimestampImpl arity=3
-- @inline export writeTimestamp arity=3
-- @inline export resolveQuerySetImpl arity=6
-- @inline export resolveQuerySet arity=6
-- @inline export finishImpl arity=1
-- @inline export finish arity=1
-- @inline export pushDebugGroupImpl arity=2
-- @inline export pushDebugGroup arity=2
-- @inline export popDebugGroupImpl arity=1
-- @inline export popDebugGroup arity=1
-- @inline export insertDebugMarkerImpl arity=2
-- @inline export insertDebugMarker arity=2
module Web.GPU.GPUCommandEncoder
  ( beginComputePass
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
import Web.GPU.GPUComputePassDescriptor (GPUComputePassDescriptor)
import Web.GPU.GPUImageCopyBuffer (GPUImageCopyBuffer)
import Web.GPU.GPUImageCopyTexture (GPUImageCopyTexture)
import Web.GPU.GPURenderPassDescriptor (GPURenderPassDescriptor)
import Web.GPU.Internal.Types (GPUBuffer, GPUCommandBuffer, GPUCommandEncoder, GPUComputePassEncoder, GPUExtent3D, GPUQuerySet, GPURenderPassEncoder, GPUSize32, GPUSize64)

foreign import beginRenderPassImpl
  :: GPUCommandEncoder
  -> GPURenderPassDescriptor
  -> Effect GPURenderPassEncoder

beginRenderPass
  :: GPUCommandEncoder -> GPURenderPassDescriptor -> Effect GPURenderPassEncoder
beginRenderPass = beginRenderPassImpl

foreign import beginComputePassImpl
  :: GPUCommandEncoder
  -> GPUComputePassDescriptor
  -> GPUComputePassEncoder

beginComputePass
  :: GPUCommandEncoder -> GPUComputePassDescriptor -> GPUComputePassEncoder
beginComputePass = beginComputePassImpl

foreign import copyBufferToBufferImpl
  :: GPUCommandEncoder
  -> GPUBuffer
  -> GPUSize64
  -> GPUBuffer
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit

copyBufferToBuffer
  :: GPUCommandEncoder
  -> GPUBuffer
  -> Int
  -> GPUBuffer
  -> Int
  -> Int
  -> Effect Unit
copyBufferToBuffer = copyBufferToBufferImpl

foreign import copyBufferToTextureImpl
  :: GPUCommandEncoder
  -> GPUImageCopyBuffer
  -> GPUImageCopyTexture
  -> GPUExtent3D
  -> Effect Unit

copyBufferToTexture
  ∷ GPUCommandEncoder
  → GPUImageCopyBuffer
  → GPUImageCopyTexture
  → GPUExtent3D
  → Effect Unit
copyBufferToTexture = copyBufferToTextureImpl

foreign import copyTextureToBufferImpl
  :: GPUCommandEncoder
  -> GPUImageCopyTexture
  -> GPUImageCopyBuffer
  -> GPUExtent3D
  -> Effect Unit

copyTextureToBuffer
  :: GPUCommandEncoder
  -> GPUImageCopyTexture
  -> GPUImageCopyBuffer
  -> GPUExtent3D
  -> Effect Unit
copyTextureToBuffer = copyTextureToBufferImpl

foreign import copyTextureToTextureImpl
  :: GPUCommandEncoder
  -> GPUImageCopyTexture
  -> GPUImageCopyTexture
  -> GPUExtent3D
  -> Effect Unit

copyTextureToTexture
  :: GPUCommandEncoder
  -> GPUImageCopyTexture
  -> GPUImageCopyTexture
  -> GPUExtent3D
  -> Effect Unit
copyTextureToTexture = copyTextureToTextureImpl

foreign import clearBufferImpl
  :: GPUCommandEncoder
  -> GPUBuffer
  -> Effect Unit

clearBuffer :: GPUCommandEncoder -> GPUBuffer -> Effect Unit
clearBuffer = clearBufferImpl

foreign import clearBufferWithOffsetImpl
  :: GPUCommandEncoder
  -> GPUBuffer
  -> GPUSize64
  -> Effect Unit

clearBufferWithOffset :: GPUCommandEncoder -> GPUBuffer -> Int -> Effect Unit
clearBufferWithOffset = clearBufferWithOffsetImpl

foreign import clearBufferWithSizeImpl
  :: GPUCommandEncoder
  -> GPUBuffer
  -> GPUSize64
  -> Effect Unit

clearBufferWithSize :: GPUCommandEncoder -> GPUBuffer -> Int -> Effect Unit
clearBufferWithSize = clearBufferWithSizeImpl

foreign import clearBufferWithOffsetAndSizeImpl
  :: GPUCommandEncoder
  -> GPUBuffer
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit

clearBufferWithOffsetAndSize
  :: GPUCommandEncoder -> GPUBuffer -> Int -> Int -> Effect Unit
clearBufferWithOffsetAndSize = clearBufferWithOffsetAndSizeImpl

foreign import writeTimestampImpl
  :: GPUCommandEncoder -> GPUQuerySet -> GPUSize32 -> Effect Unit

writeTimestamp :: GPUCommandEncoder -> GPUQuerySet -> Int -> Effect Unit
writeTimestamp = writeTimestampImpl

foreign import resolveQuerySetImpl
  :: GPUCommandEncoder
  -> GPUQuerySet
  -> GPUSize32
  -> GPUSize32
  -> GPUBuffer
  -> GPUSize64
  -> Effect Unit

resolveQuerySet
  :: GPUCommandEncoder
  -> GPUQuerySet
  -> Int
  -> Int
  -> GPUBuffer
  -> Int
  -> Effect Unit
resolveQuerySet = resolveQuerySetImpl

foreign import finishImpl :: GPUCommandEncoder -> Effect GPUCommandBuffer

finish :: GPUCommandEncoder -> Effect GPUCommandBuffer
finish = finishImpl

foreign import pushDebugGroupImpl :: GPUCommandEncoder -> String -> Effect Unit

pushDebugGroup :: GPUCommandEncoder -> String -> Effect Unit
pushDebugGroup = pushDebugGroupImpl

foreign import popDebugGroupImpl :: GPUCommandEncoder -> Effect Unit

popDebugGroup :: GPUCommandEncoder -> Effect Unit
popDebugGroup = popDebugGroupImpl

foreign import insertDebugMarkerImpl
  :: GPUCommandEncoder -> String -> Effect Unit

insertDebugMarker :: GPUCommandEncoder -> String -> Effect Unit
insertDebugMarker = insertDebugMarkerImpl