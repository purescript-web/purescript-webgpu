module WebGPU
  ( BufferDescriptor
  , TextureDescriptor
  , createBuffer
  , createTexture
  , destroy
  , features
  , limits
  , queue
  ) where

import Prelude

import Data.Set as Set
import Effect (Effect)
import Web.GPU.GPUBufferUsage (GPUBufferUsage)
import Web.GPU.GPUExtent3D (class AsGPUExtent3D, asGPUExtent3D)
import Web.GPU.GPUFeatureName (GPUFeatureName)
import Web.GPU.GPUSupportedLimits (GPUSupportedLimits)
import Web.GPU.GPUTextureDimension (GPUTextureDimension)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.GPUTextureUsage (GPUTextureUsage)
import Web.GPU.Internal.ConvertibleOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Web.GPU.Internal.Types (GPUBuffer, GPUTexture, GPUDevice, GPUQueue)
import Web.GPU.Internal.Undefinable (Undefinable, defined, undefined)
import Web.GPU.Internal.Unsigned (GPUIntegerCoordinate, GPUSize64, GPUSize32)

-- features
foreign import featuresImpl :: (GPUFeatureName -> Set.Set GPUFeatureName -> Set.Set GPUFeatureName) -> Set.Set GPUFeatureName -> GPUDevice -> Effect (Set.Set GPUFeatureName)

features :: GPUDevice -> Effect (Set.Set GPUFeatureName)
features = featuresImpl Set.insert Set.empty

-- limits
foreign import limitsImpl :: GPUDevice -> Effect { | GPUSupportedLimits }

limits :: GPUDevice -> Effect { | GPUSupportedLimits }
limits = limitsImpl

-- queue

foreign import queueImpl :: GPUDevice -> Effect GPUQueue

queue :: GPUDevice -> Effect GPUQueue
queue = queueImpl

-- destroy

foreign import destroyImpl :: GPUDevice -> Effect Unit

destroy :: GPUDevice -> Effect Unit
destroy = destroyImpl

-- createBuffer
type GPUBufferDescriptorOptional =
  ( mappedAtCreation :: Undefinable Boolean
  , label :: Undefinable String
  )

type GPUBufferDescriptor =
  ( size :: GPUSize64
  , usage :: GPUBufferUsage
  | GPUBufferDescriptorOptional
  )

defaultGPUBufferDescriptorOptions :: { | GPUBufferDescriptorOptional }
defaultGPUBufferDescriptorOptions =
  { mappedAtCreation: undefined
  , label: undefined
  }

data BufferDescriptor = BufferDescriptor

instance ConvertOption BufferDescriptor "mappedAtCreation" Boolean (Undefinable Boolean) where
  convertOption _ _ = defined

instance ConvertOption BufferDescriptor "label" String (Undefinable String) where
  convertOption _ _ = defined

foreign import createBufferImpl :: GPUDevice -> { | GPUBufferDescriptor } -> Effect GPUBuffer

createBuffer
  :: forall provided
   . ConvertOptionsWithDefaults BufferDescriptor { | GPUBufferDescriptorOptional } { | provided } { | GPUBufferDescriptor }
  => GPUDevice
  -> { | provided }
  -> Effect GPUBuffer
createBuffer gpuDevice provided = createBufferImpl gpuDevice all
  where
  all :: { | GPUBufferDescriptor }
  all = convertOptionsWithDefaults BufferDescriptor defaultGPUBufferDescriptorOptions provided

-- createTexture

type GPUTextureDescriptorOptional =
  ( mipLevelCount :: Undefinable GPUIntegerCoordinate
  , sampleCount :: Undefinable GPUSize32
  , dimension :: Undefinable GPUTextureDimension
  , viewFormats :: Undefinable (Array GPUTextureFormat)
  , label :: Undefinable String
  )

type GPUTextureDescriptor gpuExtent3D =
  ( size :: gpuExtent3D
  , usage :: GPUTextureUsage
  , format :: GPUTextureFormat
  | GPUTextureDescriptorOptional
  )

defaultGPUTextureDescriptorOptions :: { | GPUTextureDescriptorOptional }
defaultGPUTextureDescriptorOptions =
  { mipLevelCount: undefined
  , sampleCount: undefined
  , dimension: undefined
  , viewFormats: undefined
  , label: undefined
  }

data TextureDescriptor (gpuExtent3D :: Type) = TextureDescriptor

instance AsGPUExtent3D gpuExtent3DProvided gpuExtent3D => ConvertOption (TextureDescriptor gpuExtent3D) "size" gpuExtent3DProvided gpuExtent3D where
  convertOption _ _ = asGPUExtent3D

instance ConvertOption (TextureDescriptor gpuExtent3D) "mipLevelCount" GPUIntegerCoordinate (Undefinable GPUIntegerCoordinate) where
  convertOption _ _ = defined

instance ConvertOption (TextureDescriptor gpuExtent3D) "sampleCount" GPUSize32 (Undefinable GPUSize32) where
  convertOption _ _ = defined

instance ConvertOption (TextureDescriptor gpuExtent3D) "dimension" GPUTextureDimension (Undefinable GPUTextureDimension) where
  convertOption _ _ = defined

instance ConvertOption (TextureDescriptor gpuExtent3D) "viewFormats" (Array GPUTextureFormat) (Undefinable (Array GPUTextureFormat)) where
  convertOption _ _ = defined

instance ConvertOption (TextureDescriptor gpuExtent3D) "label" String (Undefinable String) where
  convertOption _ _ = defined

foreign import createTextureImpl :: forall gpuExtent3D. GPUDevice -> { | GPUTextureDescriptor gpuExtent3D } -> Effect GPUTexture

createTexture
  :: forall provided gpuExtent3D
   . ConvertOptionsWithDefaults (TextureDescriptor gpuExtent3D) { | GPUTextureDescriptorOptional } { | provided } { | GPUTextureDescriptor gpuExtent3D }
  => GPUDevice
  -> { | provided }
  -> Effect GPUTexture
createTexture gpuDevice provided = createTextureImpl gpuDevice all
  where
  all :: { | GPUTextureDescriptor gpuExtent3D }
  all = convertOptionsWithDefaults
    (TextureDescriptor :: TextureDescriptor gpuExtent3D)
    defaultGPUTextureDescriptorOptions
    provided