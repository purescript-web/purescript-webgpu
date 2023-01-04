module Web.GPU.GPUCommandEncoder where

import Prelude

import Effect (Effect)
import Web.GPU.GPUComputePassTimestampLocation (GPUComputePassTimestampLocation)
import Web.GPU.GPULoadOp (GPULoadOp)
import Web.GPU.GPURenderPassTimestampLocation (GPURenderPassTimestampLocation)
import Web.GPU.GPUStoreOp (GPUStoreOp)
import Web.GPU.GPUTextureAspect (GPUTextureAspect)
import Web.GPU.Internal.ConvertibleOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Web.GPU.Internal.Types (GPUBuffer, GPUColor, GPUCommandBuffer, GPUCommandEncoder, GPUComputePassEncoder, GPUExtent3D, GPUOrigin3D, GPUQuerySet, GPURenderPassEncoder, GPUTexture, GPUTextureView)
import Web.GPU.Internal.Undefinable (Undefinable, defined, undefined)
import Web.GPU.Internal.Unsigned (GPUSize32, GPUSize64, GPUStencilValue, GPUIntegerCoordinate)

-- todo
-- convertible options on GPURenderPassDescriptor

type GPURenderPassTimestampWrite =
  { querySet :: GPUQuerySet
  , queryIndex :: GPUSize32
  , location :: GPURenderPassTimestampLocation
  }

type GPURenderPassTimestampWrites = Array GPURenderPassTimestampWrite

type GPURenderPassDescriptorOptional :: forall k. k -> Row Type
type GPURenderPassDescriptorOptional gpuRenderPassDepthStencilAttachment =
  ( depthStencilAttachment :: Undefinable gpuRenderPassDepthStencilAttachment
  , occlusionQuerySet :: Undefinable GPUQuerySet
  , timestampWrites :: Undefinable GPURenderPassTimestampWrites
  , maxDrawCount :: Undefinable GPUSize64
  )

type GPURenderPassColorAttachmentOptional =
  ( resolveTarget :: Undefinable GPUTextureView
  , clearValue :: Undefinable GPUColor
  )

type GPURenderPassColorAttachment =
  ( view :: GPUTextureView
  , loadOp :: GPULoadOp
  , storeOp :: GPUStoreOp
  | GPURenderPassColorAttachmentOptional
  )

defaultGPURenderPassColorAttachmentOptions :: { | GPURenderPassColorAttachmentOptional }
defaultGPURenderPassColorAttachmentOptions =
  { resolveTarget: undefined
  , clearValue: undefined
  }

data RenderPassColorAttachment = RenderPassColorAttachment

instance ConvertOption RenderPassColorAttachment "resolveTarget" GPUTextureView (Undefinable GPUTextureView) where
  convertOption _ _ = defined

instance ConvertOption RenderPassColorAttachment "clearValue" GPUColor (Undefinable GPUColor) where
  convertOption _ _ = defined

gpuRenderPassColorAttachment
  :: forall provided
   . ConvertOptionsWithDefaults RenderPassColorAttachment { | GPURenderPassColorAttachmentOptional } { | provided } { | GPURenderPassColorAttachment }
  => { | provided }
  -> { | GPURenderPassColorAttachment }

gpuRenderPassColorAttachment provided = all
  where
  all :: { | GPURenderPassColorAttachment }
  all = convertOptionsWithDefaults RenderPassColorAttachment defaultGPURenderPassColorAttachmentOptions provided

type GPURenderPassDepthStencilAttachmentOptional =
  ( depthClearValue :: Undefinable Number
  , depthReadOnly :: Undefinable Boolean
  , stencilClearValue :: Undefinable GPUStencilValue
  , stencilReadOnly :: Undefinable Boolean
  )

type GPURenderPassDepthStencilAttachment =
  ( view :: GPUTextureView
  , depthLoadOp :: GPULoadOp
  , depthStoreOp :: GPUStoreOp
  , stencilLoadOp :: GPULoadOp
  , stencilStoreOp :: GPUStoreOp
  | GPURenderPassDepthStencilAttachmentOptional
  )

type GPURenderPassDescriptor :: forall k. k -> Row Type
type GPURenderPassDescriptor gpuRenderPassDepthStencilAttachment =
  ( colorAttachments :: Array { | GPURenderPassColorAttachment }
  | GPURenderPassDescriptorOptional gpuRenderPassDepthStencilAttachment
  )

type GPUComputePassTimestampWrite =
  { querySet :: GPUQuerySet
  , queryIndex :: GPUSize32
  , location :: GPUComputePassTimestampLocation
  }

type GPUComputePassTimestampWrites = Array GPUComputePassTimestampWrite
type GPUComputePassDescriptorOptional =
  ( timestampWrites :: Undefinable GPUComputePassTimestampWrites
  )

type GPUComputePassDescriptor =
  (
  | GPUComputePassDescriptorOptional
  )

foreign import beginRenderPassImpl :: GPUCommandEncoder -> { | GPURenderPassDescriptor GPURenderPassDepthStencilAttachment } -> Effect GPURenderPassEncoder
foreign import beginComputePassImpl :: GPUCommandEncoder -> { | GPUComputePassDescriptor } -> GPUComputePassEncoder
foreign import copyBufferToBufferImpl
  :: GPUCommandEncoder
  -> GPUBuffer
  -> GPUSize64
  -> GPUBuffer
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit

type GPUImageCopyBufferOptional =
  ( offset :: GPUSize64
  , bytesPerRow :: GPUSize32
  , rowsPerImage :: GPUSize32
  )

type GPUImageCopyBuffer =
  ( buffer :: GPUBuffer
  | GPUImageCopyBufferOptional
  )

type GPUImageCopyTextureOptional =
  ( mipLevel :: GPUIntegerCoordinate
  , origin :: GPUOrigin3D
  , aspect :: GPUTextureAspect
  )

type GPUImageCopyTexture =
  ( texture :: GPUTexture
  | GPUImageCopyTextureOptional
  )

foreign import copyBufferToTextureImpl
  :: GPUCommandEncoder
  -> { | GPUImageCopyBuffer }
  -> { | GPUImageCopyTexture }
  -> GPUExtent3D
  -> Effect Unit

foreign import copyTextureToBufferImpl
  :: GPUCommandEncoder
  -> { | GPUImageCopyTexture }
  -> { | GPUImageCopyBuffer }
  -> GPUExtent3D
  -> Effect Unit

foreign import copyTextureToTextureImpl
  :: GPUCommandEncoder
  -> { | GPUImageCopyTexture }
  -> { | GPUImageCopyTexture }
  -> GPUExtent3D
  -> Effect Unit

foreign import clearBufferImpl
  :: GPUCommandEncoder
  -> GPUBuffer
  -> Effect Unit

foreign import clearBufferWithOffsetImpl
  :: GPUCommandEncoder
  -> GPUBuffer
  -> GPUSize64
  -> Effect Unit

foreign import clearBufferWithSizeImpl
  :: GPUCommandEncoder
  -> GPUBuffer
  -> GPUSize64
  -> Effect Unit

foreign import clearBufferWithOffsetAndSizeImpl
  :: GPUCommandEncoder
  -> GPUBuffer
  -> GPUSize64
  -> GPUSize64
  -> Effect Unit

foreign import writeTimestampImpl :: GPUCommandEncoder -> GPUQuerySet -> GPUSize32 -> Effect Unit

foreign import resolveQuerySetImpl
  :: GPUCommandEncoder
  -> GPUQuerySet
  -> GPUSize32
  -> GPUSize32
  -> GPUBuffer
  -> GPUSize64
  -> Effect Unit

foreign import finishImpl :: GPUCommandEncoder -> Effect GPUCommandBuffer

foreign import pushDebugGroupImpl :: GPUCommandEncoder -> String -> Effect Unit
foreign import popDebugGroupImpl :: GPUCommandEncoder -> Effect Unit
foreign import insertDebugMarkerImpl :: GPUCommandEncoder -> String -> Effect Unit