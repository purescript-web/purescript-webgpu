module Web.GPU.GPUQueue
  ( copyExternalImageToTexture
  , onSubmittedWorkDone
  , submit
  , writeBuffer
  , writeBufferWithOffset
  , writeBufferWithOffsetAndSize
  , writeBufferWithSize
  , writeTexture
  )
  where

import Prelude

import Effect (Effect)
import Web.GPU.GPUImageCopyExternalImage (GPUImageCopyExternalImage)
import Web.GPU.GPUImageCopyTexture (GPUImageCopyTexture)
import Web.GPU.GPUImageCopyTextureTagged (GPUImageCopyTextureTagged)
import Web.GPU.GPUImageDataLayout (GPUImageDataLayout)
import Web.GPU.Internal.Types (BufferSource, GPUBuffer, GPUCommandBuffer, GPUExtent3D, GPUQueue, GPUSize64)
import Web.Promise (Promise)

foreign import submitImpl ::  GPUQueue -> Array GPUCommandBuffer -> Effect Unit
submit ∷ GPUQueue → Array GPUCommandBuffer → Effect Unit
submit = submitImpl
foreign import onSubmittedWorkDoneImpl ::  GPUQueue -> Effect (Promise Unit)
onSubmittedWorkDone :: GPUQueue -> Effect (Promise Unit)
onSubmittedWorkDone = onSubmittedWorkDoneImpl
foreign import writeBufferImpl :: GPUQueue -> GPUBuffer -> GPUSize64 -> BufferSource -> Effect Unit
writeBuffer :: GPUQueue -> GPUBuffer -> Int -> BufferSource -> Effect Unit
writeBuffer = writeBufferImpl
foreign import writeBufferWithOffsetImpl :: GPUQueue -> GPUBuffer -> GPUSize64 -> BufferSource -> GPUSize64 -> Effect Unit
writeBufferWithOffset :: GPUQueue -> GPUBuffer -> Int -> BufferSource -> Int -> Effect Unit
writeBufferWithOffset = writeBufferWithOffsetImpl
foreign import writeBufferWithSizeImpl :: GPUQueue -> GPUBuffer -> GPUSize64 -> BufferSource -> GPUSize64 -> Effect Unit
writeBufferWithSize :: GPUQueue -> GPUBuffer -> Int -> BufferSource -> Int -> Effect Unit
writeBufferWithSize = writeBufferWithSizeImpl
foreign import writeBufferWithOffsetAndSizeImpl :: GPUQueue -> GPUBuffer -> GPUSize64 -> BufferSource -> GPUSize64 -> GPUSize64 -> Effect Unit
writeBufferWithOffsetAndSize :: GPUQueue -> GPUBuffer -> Int -> BufferSource -> Int -> Int -> Effect Unit
writeBufferWithOffsetAndSize = writeBufferWithOffsetAndSizeImpl
foreign import writeTextureImpl :: GPUQueue -> GPUImageCopyTexture -> BufferSource -> GPUImageDataLayout -> GPUExtent3D -> Effect Unit
writeTexture :: GPUQueue -> GPUImageCopyTexture -> BufferSource -> GPUImageDataLayout -> GPUExtent3D -> Effect Unit
writeTexture = writeTextureImpl
foreign import copyExternalImageToTextureImpl :: GPUQueue -> GPUImageCopyExternalImage -> GPUImageCopyTextureTagged -> GPUExtent3D -> Effect Unit
copyExternalImageToTexture :: GPUQueue -> GPUImageCopyExternalImage -> GPUImageCopyTextureTagged -> GPUExtent3D -> Effect Unit
copyExternalImageToTexture = copyExternalImageToTextureImpl
