-- @inline export submitImpl arity=2
-- @inline export onSubmittedWorkDoneImpl arity=1
-- @inline export onSubmittedWorkDone arity=1
-- @inline export writeBufferImpl arity=4
-- @inline export writeBuffer arity=4
-- @inline export writeBufferWithOffsetImpl arity=5
-- @inline export writeBufferWithOffset arity=5
-- @inline export writeBufferWithSizeImpl arity=5
-- @inline export writeBufferWithSize arity=5
-- @inline export writeBufferWithOffsetAndSizeImpl arity=6
-- @inline export writeBufferWithOffsetAndSize arity=6
-- @inline export writeTextureImpl arity=5
-- @inline export writeTexture arity=5
-- @inline export copyExternalImageToTextureImpl arity=4
-- @inline export copyExternalImageToTexture arity=4
module Web.GPU.GPUQueue
  ( GPUQueue
  , copyExternalImageToTexture
  , onSubmittedWorkDone
  , submit
  , writeBuffer
  , writeBufferWithOffset
  , writeBufferWithOffsetAndSize
  , writeBufferWithSize
  , writeTexture
  ) where

import Prelude

import Effect (Effect)
import Web.GPU.BufferSource (BufferSource)
import Web.GPU.GPUBuffer (GPUBuffer)
import Web.GPU.GPUExtent3D (GPUExtent3D)
import Web.GPU.GPUImageCopyExternalImage (GPUImageCopyExternalImage)
import Web.GPU.GPUImageCopyTexture (GPUImageCopyTexture)
import Web.GPU.GPUImageCopyTextureTagged (GPUImageCopyTextureTagged)
import Web.GPU.GPUImageDataLayout (GPUImageDataLayout)
import Web.GPU.Internal.GPUCommandBuffer (GPUCommandBuffer)
import Web.GPU.Internal.Types (GPUSize64)
import Web.Promise (Promise)

data GPUQueue

foreign import submitImpl :: GPUQueue -> Array GPUCommandBuffer -> Effect Unit

submit ∷ GPUQueue → Array GPUCommandBuffer → Effect Unit
submit = submitImpl

foreign import onSubmittedWorkDoneImpl :: GPUQueue -> Effect (Promise Unit)

onSubmittedWorkDone :: GPUQueue -> Effect (Promise Unit)
onSubmittedWorkDone = onSubmittedWorkDoneImpl

foreign import writeBufferImpl
  :: GPUQueue -> GPUBuffer -> GPUSize64 -> BufferSource -> Effect Unit

writeBuffer :: GPUQueue -> GPUBuffer -> Int -> BufferSource -> Effect Unit
writeBuffer = writeBufferImpl

foreign import writeBufferWithOffsetImpl
  :: GPUQueue
  -> GPUBuffer
  -> GPUSize64
  -> BufferSource
  -> GPUSize64
  -> Effect Unit

writeBufferWithOffset
  :: GPUQueue -> GPUBuffer -> Int -> BufferSource -> Int -> Effect Unit
writeBufferWithOffset = writeBufferWithOffsetImpl

foreign import writeBufferWithSizeImpl
  :: GPUQueue
  -> GPUBuffer
  -> GPUSize64
  -> BufferSource
  -> GPUSize64
  -> Effect Unit

writeBufferWithSize
  :: GPUQueue -> GPUBuffer -> Int -> BufferSource -> Int -> Effect Unit
writeBufferWithSize = writeBufferWithSizeImpl

foreign import writeBufferWithOffsetAndSizeImpl
  :: GPUQueue
  -> GPUBuffer
  -> GPUSize64
  -> BufferSource
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit

writeBufferWithOffsetAndSize
  :: GPUQueue -> GPUBuffer -> Int -> BufferSource -> Int -> Int -> Effect Unit
writeBufferWithOffsetAndSize = writeBufferWithOffsetAndSizeImpl

foreign import writeTextureImpl
  :: GPUQueue
  -> GPUImageCopyTexture
  -> BufferSource
  -> GPUImageDataLayout
  -> GPUExtent3D
  -> Effect Unit

writeTexture
  :: GPUQueue
  -> GPUImageCopyTexture
  -> BufferSource
  -> GPUImageDataLayout
  -> GPUExtent3D
  -> Effect Unit
writeTexture = writeTextureImpl

foreign import copyExternalImageToTextureImpl
  :: GPUQueue
  -> GPUImageCopyExternalImage
  -> GPUImageCopyTextureTagged
  -> GPUExtent3D
  -> Effect Unit

copyExternalImageToTexture
  :: GPUQueue
  -> GPUImageCopyExternalImage
  -> GPUImageCopyTextureTagged
  -> GPUExtent3D
  -> Effect Unit
copyExternalImageToTexture = copyExternalImageToTextureImpl