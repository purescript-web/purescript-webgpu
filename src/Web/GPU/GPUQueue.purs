-- @inline export onSubmittedWorkDone arity=1
-- @inline export writeBuffer arity=4
-- @inline export writeBufferWithOffset arity=5
-- @inline export writeBufferWithSize arity=5
-- @inline export writeBufferWithOffsetAndSize arity=6
-- @inline export writeTexture arity=5
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
import Effect.Uncurried (EffectFn1, runEffectFn1, EffectFn2, runEffectFn2, EffectFn4, runEffectFn4, EffectFn5, runEffectFn5, EffectFn6, runEffectFn6)

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
import Promise (Promise)

data GPUQueue

foreign import submitImpl :: EffectFn2 GPUQueue (Array GPUCommandBuffer) Unit

submit ∷ GPUQueue → Array GPUCommandBuffer → Effect Unit
submit a b = runEffectFn2 submitImpl a b

foreign import onSubmittedWorkDoneImpl :: EffectFn1 GPUQueue (Promise Unit)

onSubmittedWorkDone :: GPUQueue -> Effect (Promise Unit)
onSubmittedWorkDone a = runEffectFn1 onSubmittedWorkDoneImpl a

foreign import writeBufferImpl
  :: EffectFn4 GPUQueue GPUBuffer GPUSize64 BufferSource Unit

writeBuffer :: GPUQueue -> GPUBuffer -> Int -> BufferSource -> Effect Unit
writeBuffer a b c d = runEffectFn4 writeBufferImpl a b c d

foreign import writeBufferWithOffsetImpl
  :: EffectFn5 GPUQueue GPUBuffer GPUSize64 BufferSource GPUSize64 Unit

writeBufferWithOffset
  :: GPUQueue -> GPUBuffer -> Int -> BufferSource -> Int -> Effect Unit
writeBufferWithOffset a b c d e = runEffectFn5 writeBufferWithOffsetImpl a b c d
  e

foreign import writeBufferWithSizeImpl
  :: EffectFn5 GPUQueue GPUBuffer GPUSize64 BufferSource GPUSize64 Unit

writeBufferWithSize
  :: GPUQueue -> GPUBuffer -> Int -> BufferSource -> Int -> Effect Unit
writeBufferWithSize a b c d e = runEffectFn5 writeBufferWithSizeImpl a b c d e

foreign import writeBufferWithOffsetAndSizeImpl
  :: EffectFn6 GPUQueue GPUBuffer GPUSize64 BufferSource GPUSize64 GPUSize64
       Unit

writeBufferWithOffsetAndSize
  :: GPUQueue -> GPUBuffer -> Int -> BufferSource -> Int -> Int -> Effect Unit
writeBufferWithOffsetAndSize a b c d e f = runEffectFn6
  writeBufferWithOffsetAndSizeImpl
  a
  b
  c
  d
  e
  f

foreign import writeTextureImpl
  :: EffectFn5 GPUQueue GPUImageCopyTexture BufferSource GPUImageDataLayout
       GPUExtent3D
       Unit

writeTexture
  :: GPUQueue
  -> GPUImageCopyTexture
  -> BufferSource
  -> GPUImageDataLayout
  -> GPUExtent3D
  -> Effect Unit
writeTexture a b c d e = runEffectFn5 writeTextureImpl a b c d e

foreign import copyExternalImageToTextureImpl
  :: EffectFn4 GPUQueue GPUImageCopyExternalImage GPUImageCopyTextureTagged
       GPUExtent3D
       Unit

copyExternalImageToTexture
  :: GPUQueue
  -> GPUImageCopyExternalImage
  -> GPUImageCopyTextureTagged
  -> GPUExtent3D
  -> Effect Unit
copyExternalImageToTexture a b c d = runEffectFn4 copyExternalImageToTextureImpl
  a
  b
  c
  d
