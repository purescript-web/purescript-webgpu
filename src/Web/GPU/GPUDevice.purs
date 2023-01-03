module Web.GPU.Device
  ( BufferBindingLayout
  , BufferDescriptor
  , ExternalTextureDescriptor
  , SamplerBindingLayout
  , SamplerDescriptor
  , StorageTextureBindingLayout
  , TextureBindingLayout
  , TextureDescriptor
  , ExternalTextureBindingLayout
  , bindGroupLayoutEntryForBuffer
  , bindGroupLayoutEntryForExternalTexture
  , bindGroupLayoutEntryForSampler
  , bindGroupLayoutEntryForStorageTexture
  , bindGroupLayoutEntryForTexture
  , createBindGroupLayout
  , createBuffer
  , createPipelineLayout
  , createSampler
  , createTexture
  , destroy
  , features
  , importExternalTexture
  , limits
  , queue
  ) where

import Prelude

import Data.Reflectable (class Reflectable, reflectType)
import Data.Set as Set
import Effect (Effect)
import Prim.Int (class Compare)
import Prim.Ordering (LT)
import Type.Proxy (Proxy)
import Web.GPU.GPUAddressMode (GPUAddressMode)
import Web.GPU.GPUBufferBindingType (GPUBufferBindingType)
import Web.GPU.GPUBufferUsage (GPUBufferUsage)
import Web.GPU.GPUCompareFunction (GPUCompareFunction)
import Web.GPU.GPUExtent3D (class AsGPUExtent3D, asGPUExtent3D)
import Web.GPU.GPUFeatureName (GPUFeatureName)
import Web.GPU.GPUFilterMode (GPUFilterMode)
import Web.GPU.GPUMipmapFilterMode (GPUMipmapFilterMode)
import Web.GPU.GPUSamplerBindingType (GPUSamplerBindingType)
import Web.GPU.GPUShaderStage (GPUShaderStage)
import Web.GPU.GPUStorageTextureAccess (GPUStorageTextureAccess)
import Web.GPU.GPUSupportedLimits (GPUSupportedLimits)
import Web.GPU.GPUTextureDimension (GPUTextureDimension)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.GPUTextureSampleType (GPUTextureSampleType)
import Web.GPU.GPUTextureUsage (GPUTextureUsage)
import Web.GPU.GPUTextureViewDimension (GPUTextureViewDimension)
import Web.GPU.Internal.ConvertibleOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Web.GPU.Internal.Types (GPUBindGroupLayout, GPUBindGroupLayoutDescriptor, GPUBindGroupLayoutEntry, GPUBuffer, GPUBufferBindingLayout, GPUDevice, GPUExternalTexture, GPUExternalTextureBindingLayout, GPUPipelineLayout, GPUPipelineLayoutDescriptor, GPUQueue, GPUSampler, GPUSamplerBindingLayout, GPUStorageTextureBindingLayout, GPUTexture, GPUTextureBindingLayout)
import Web.GPU.Internal.Undefinable (Undefinable, defined, undefined)
import Web.GPU.Internal.Unsigned (GPUIntegerCoordinate, GPUSize32, GPUSize64, UnsignedShort, GPUIndex32)
import Web.GPU.PredefinedColorSpace (PredefinedColorSpace)
import Web.HTML (HTMLVideoElement)

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

-- createSampler
type GPUSamplerDescriptorOptional =
  ( addressModeU :: Undefinable GPUAddressMode
  , addressModeV :: Undefinable GPUAddressMode
  , addressModeW :: Undefinable GPUAddressMode
  , magFilter :: Undefinable GPUFilterMode
  , minFilter :: Undefinable GPUFilterMode
  , mipmapFilter :: Undefinable GPUMipmapFilterMode
  , lodMinClamp :: Undefinable Number
  , lodMaxClamp :: Undefinable Number
  , compare :: Undefinable GPUCompareFunction
  , maxAnisotropy :: Undefinable UnsignedShort
  )

type GPUSamplerDescriptor =
  ( | GPUSamplerDescriptorOptional
  )

defaultGPUSamplerDescriptorOptions :: { | GPUSamplerDescriptorOptional }
defaultGPUSamplerDescriptorOptions =
  { addressModeU: undefined
  , addressModeV: undefined
  , addressModeW: undefined
  , magFilter: undefined
  , minFilter: undefined
  , mipmapFilter: undefined
  , lodMinClamp: undefined
  , lodMaxClamp: undefined
  , compare: undefined
  , maxAnisotropy: undefined
  }

data SamplerDescriptor = SamplerDescriptor

instance ConvertOption SamplerDescriptor "addressModeU" GPUAddressMode (Undefinable GPUAddressMode) where
  convertOption _ _ = defined

instance ConvertOption SamplerDescriptor "addressModeV" GPUAddressMode (Undefinable GPUAddressMode) where
  convertOption _ _ = defined

instance ConvertOption SamplerDescriptor "addressModeW" GPUAddressMode (Undefinable GPUAddressMode) where
  convertOption _ _ = defined

instance ConvertOption SamplerDescriptor "magFilter" GPUFilterMode (Undefinable GPUFilterMode) where
  convertOption _ _ = defined

instance ConvertOption SamplerDescriptor "minFilter" GPUFilterMode (Undefinable GPUFilterMode) where
  convertOption _ _ = defined

instance ConvertOption SamplerDescriptor "mipmapFilter" GPUMipmapFilterMode (Undefinable GPUMipmapFilterMode) where
  convertOption _ _ = defined

instance ConvertOption SamplerDescriptor "lodMinClamp" Number (Undefinable Number) where
  convertOption _ _ = defined

instance ConvertOption SamplerDescriptor "lodMaxClamp" Number (Undefinable Number) where
  convertOption _ _ = defined

instance ConvertOption SamplerDescriptor "compare" GPUCompareFunction (Undefinable GPUCompareFunction) where
  convertOption _ _ = defined

instance ConvertOption SamplerDescriptor "maxAnisotropy" UnsignedShort (Undefinable UnsignedShort) where
  convertOption _ _ = defined

foreign import createSamplerImpl :: GPUDevice -> { | GPUSamplerDescriptor } -> Effect GPUSampler

createSampler
  :: forall provided
   . ConvertOptionsWithDefaults SamplerDescriptor { | GPUSamplerDescriptorOptional } { | provided } { | GPUSamplerDescriptor }
  => GPUDevice
  -> { | provided }
  -> Effect GPUSampler
createSampler gpuDevice provided = createSamplerImpl gpuDevice all
  where
  all :: { | GPUSamplerDescriptor }
  all = convertOptionsWithDefaults SamplerDescriptor defaultGPUSamplerDescriptorOptions provided

-- importExternalTexture

type GPUExternalTextureDescriptorOptional =
  ( colorSpace :: Undefinable PredefinedColorSpace
  )

type GPUExternalTextureDescriptor =
  ( source :: HTMLVideoElement
  | GPUExternalTextureDescriptorOptional
  )

defaultGPUExternalTextureDescriptorOptions :: { | GPUExternalTextureDescriptorOptional }
defaultGPUExternalTextureDescriptorOptions =
  { colorSpace: undefined
  }

data ExternalTextureDescriptor = ExternalTextureDescriptor

instance ConvertOption ExternalTextureDescriptor "colorSpace" PredefinedColorSpace (Undefinable PredefinedColorSpace) where
  convertOption _ _ = defined

foreign import importExternalTextureImpl :: GPUDevice -> { | GPUExternalTextureDescriptor } -> Effect GPUExternalTexture

importExternalTexture
  :: forall provided
   . ConvertOptionsWithDefaults ExternalTextureDescriptor { | GPUExternalTextureDescriptorOptional } { | provided } { | GPUExternalTextureDescriptor }
  => GPUDevice
  -> { | provided }
  -> Effect GPUExternalTexture
importExternalTexture gpuDevice provided = importExternalTextureImpl gpuDevice all
  where
  all :: { | GPUExternalTextureDescriptor }
  all = convertOptionsWithDefaults ExternalTextureDescriptor defaultGPUExternalTextureDescriptorOptions provided

-- createBindGroupLayout

bindingToGPUIndex32 :: forall i. Reflectable i Int => Proxy i -> GPUIndex32
bindingToGPUIndex32 = reflectType

type GPUBindGroupLayoutOptions =
  { binding :: GPUIndex32
  , visibility :: GPUShaderStage
  }

foreign import createBindGroupLayoutImpl :: forall entries. GPUDevice -> GPUBindGroupLayoutDescriptor entries -> Effect (GPUBindGroupLayout entries)

createBindGroupLayout
  :: forall entries
   . GPUDevice
  -> GPUBindGroupLayoutDescriptor entries
  -> Effect (GPUBindGroupLayout entries)
createBindGroupLayout = createBindGroupLayoutImpl

---- bindGroupLayoutEntryForBuffer

type GPUBufferBindingLayoutOptional =
  ( type :: Undefinable GPUBufferBindingType
  , hasDynamicOffset :: Undefinable Boolean
  , minBindingSize :: Undefinable GPUSize64
  )

type GPUBufferBindingLayoutAll =
  (| GPUBufferBindingLayoutOptional)

defaultGPUBufferBindingLayoutOptions :: { | GPUBufferBindingLayoutOptional }
defaultGPUBufferBindingLayoutOptions =
  { type: undefined
  , hasDynamicOffset: undefined
  , minBindingSize: undefined
  }

data BufferBindingLayout = BufferBindingLayout

instance ConvertOption BufferBindingLayout "type" GPUBufferBindingType (Undefinable GPUBufferBindingType) where
  convertOption _ _ = defined

instance ConvertOption BufferBindingLayout "hasDynamicOffset" Boolean (Undefinable Boolean) where
  convertOption _ _ = defined

instance ConvertOption BufferBindingLayout "minBindingSize" GPUSize64 (Undefinable GPUSize64) where
  convertOption _ _ = defined

foreign import bindGroupLayoutEntryForBufferImpl :: forall i g. GPUBindGroupLayoutOptions -> { | GPUBufferBindingLayoutAll } -> GPUBindGroupLayoutEntry i g

bindGroupLayoutEntryForBuffer
  :: forall provided i
   . ConvertOptionsWithDefaults BufferBindingLayout { | GPUBufferBindingLayoutOptional } { | provided } { | GPUBufferBindingLayoutAll }
  => Reflectable i Int
  => Compare (-1) i LT
  => Proxy i
  -> GPUShaderStage
  -> { | provided }
  -> GPUBindGroupLayoutEntry i GPUBufferBindingLayout
bindGroupLayoutEntryForBuffer binding visibility provided = bindGroupLayoutEntryForBufferImpl { binding: bindingToGPUIndex32 binding, visibility } all
  where
  all :: { | GPUBufferBindingLayoutAll }
  all = convertOptionsWithDefaults BufferBindingLayout defaultGPUBufferBindingLayoutOptions provided

---- bindGroupLayoutEntryForSampler

type GPUSamplerBindingLayoutOptional =
  ( type :: Undefinable GPUSamplerBindingType
  )

type GPUSamplerBindingLayoutAll =
  (| GPUSamplerBindingLayoutOptional)

defaultGPUSamplerBindingLayoutOptions :: { | GPUSamplerBindingLayoutOptional }
defaultGPUSamplerBindingLayoutOptions =
  { type: undefined
  }

data SamplerBindingLayout = SamplerBindingLayout

instance ConvertOption SamplerBindingLayout "type" GPUSamplerBindingType (Undefinable GPUSamplerBindingType) where
  convertOption _ _ = defined

foreign import bindGroupLayoutEntryForSamplerImpl :: forall i g. GPUBindGroupLayoutOptions -> { | GPUSamplerBindingLayoutAll } -> GPUBindGroupLayoutEntry i g

bindGroupLayoutEntryForSampler
  :: forall provided i
   . ConvertOptionsWithDefaults SamplerBindingLayout { | GPUSamplerBindingLayoutOptional } { | provided } { | GPUSamplerBindingLayoutAll }
  => Reflectable i Int
  => Compare (-1) i LT
  => Proxy i
  -> GPUShaderStage
  -> { | provided }
  -> GPUBindGroupLayoutEntry i GPUSamplerBindingLayout
bindGroupLayoutEntryForSampler binding visibility provided = bindGroupLayoutEntryForSamplerImpl { binding: bindingToGPUIndex32 binding, visibility } all
  where
  all :: { | GPUSamplerBindingLayoutAll }
  all = convertOptionsWithDefaults SamplerBindingLayout defaultGPUSamplerBindingLayoutOptions provided

---- bindGroupLayoutEntryForTexture

type GPUTextureBindingLayoutOptional =
  ( sampleType :: Undefinable GPUTextureSampleType
  , viewDimension :: Undefinable GPUTextureViewDimension
  , multisampled :: Undefinable Boolean
  )

type GPUTextureBindingLayoutAll =
  (| GPUTextureBindingLayoutOptional)

defaultGPUTextureBindingLayoutOptions :: { | GPUTextureBindingLayoutOptional }
defaultGPUTextureBindingLayoutOptions =
  { sampleType: undefined
  , viewDimension: undefined
  , multisampled: undefined
  }

data TextureBindingLayout = TextureBindingLayout

instance ConvertOption TextureBindingLayout "sampleType" GPUTextureSampleType (Undefinable GPUTextureSampleType) where
  convertOption _ _ = defined

instance ConvertOption TextureBindingLayout "viewDimension" GPUTextureViewDimension (Undefinable GPUTextureViewDimension) where
  convertOption _ _ = defined

instance ConvertOption TextureBindingLayout "multisampled" Boolean (Undefinable Boolean) where
  convertOption _ _ = defined

foreign import bindGroupLayoutEntryForTextureImpl :: forall i g. GPUBindGroupLayoutOptions -> { | GPUTextureBindingLayoutAll } -> GPUBindGroupLayoutEntry i g

bindGroupLayoutEntryForTexture
  :: forall provided i
   . ConvertOptionsWithDefaults TextureBindingLayout { | GPUTextureBindingLayoutOptional } { | provided } { | GPUTextureBindingLayoutAll }
  => Reflectable i Int
  => Compare (-1) i LT
  => Proxy i
  -> GPUShaderStage
  -> { | provided }
  -> GPUBindGroupLayoutEntry i GPUTextureBindingLayout
bindGroupLayoutEntryForTexture binding visibility provided = bindGroupLayoutEntryForTextureImpl { binding: bindingToGPUIndex32 binding, visibility } all
  where
  all :: { | GPUTextureBindingLayoutAll }
  all = convertOptionsWithDefaults TextureBindingLayout defaultGPUTextureBindingLayoutOptions provided

---- bindGroupLayoutEntryForStorageTexture
type GPUStorageTextureBindingLayoutOptional =
  ( access :: Undefinable GPUStorageTextureAccess
  , viewDimension :: Undefinable GPUTextureViewDimension
  )

type GPUStorageTextureBindingLayoutAll =
  (format :: GPUTextureFormat | GPUStorageTextureBindingLayoutOptional)

defaultGPUStorageTextureBindingLayoutOptions :: { | GPUStorageTextureBindingLayoutOptional }
defaultGPUStorageTextureBindingLayoutOptions =
  { access: undefined
  , viewDimension: undefined
  }

data StorageTextureBindingLayout = StorageTextureBindingLayout

instance ConvertOption StorageTextureBindingLayout "access" GPUStorageTextureAccess (Undefinable GPUStorageTextureAccess) where
  convertOption _ _ = defined

instance ConvertOption StorageTextureBindingLayout "viewDimension" GPUTextureViewDimension (Undefinable GPUTextureViewDimension) where
  convertOption _ _ = defined

foreign import bindGroupLayoutEntryForStorageTextureImpl :: forall i g. GPUBindGroupLayoutOptions -> { | GPUStorageTextureBindingLayoutAll } -> GPUBindGroupLayoutEntry i g

bindGroupLayoutEntryForStorageTexture
  :: forall provided i
   . ConvertOptionsWithDefaults StorageTextureBindingLayout { | GPUStorageTextureBindingLayoutOptional } { | provided } { | GPUStorageTextureBindingLayoutAll }
  => Reflectable i Int
  => Compare (-1) i LT
  => Proxy i
  -> GPUShaderStage
  -> { | provided }
  -> GPUBindGroupLayoutEntry i GPUStorageTextureBindingLayout
bindGroupLayoutEntryForStorageTexture binding visibility provided = bindGroupLayoutEntryForStorageTextureImpl { binding: bindingToGPUIndex32 binding, visibility } all
  where
  all :: { | GPUStorageTextureBindingLayoutAll }
  all = convertOptionsWithDefaults StorageTextureBindingLayout defaultGPUStorageTextureBindingLayoutOptions provided

---- bindGroupLayoutEntryForExternalTexture
type GPUExternalTextureBindingLayoutOptional :: forall k. Row k
type GPUExternalTextureBindingLayoutOptional = ()

type GPUExternalTextureBindingLayoutAll :: forall k. Row k
type GPUExternalTextureBindingLayoutAll = (| GPUExternalTextureBindingLayoutOptional)

defaultGPUExternalTextureBindingLayoutOptions :: { | GPUExternalTextureBindingLayoutOptional }
defaultGPUExternalTextureBindingLayoutOptions = {}

data ExternalTextureBindingLayout = ExternalTextureBindingLayout

foreign import bindGroupLayoutEntryForExternalTextureImpl :: forall i g. GPUBindGroupLayoutOptions -> { | GPUExternalTextureBindingLayoutAll } -> GPUBindGroupLayoutEntry i g

bindGroupLayoutEntryForExternalTexture
  :: forall provided i
   . ConvertOptionsWithDefaults ExternalTextureBindingLayout { | GPUExternalTextureBindingLayoutOptional } { | provided } { | GPUExternalTextureBindingLayoutAll }
  => Reflectable i Int
  => Compare (-1) i LT
  => Proxy i
  -> GPUShaderStage
  -> { | provided }
  -> GPUBindGroupLayoutEntry i GPUExternalTextureBindingLayout
bindGroupLayoutEntryForExternalTexture binding visibility provided = bindGroupLayoutEntryForExternalTextureImpl { binding: bindingToGPUIndex32 binding, visibility } all
  where
  all :: { | GPUExternalTextureBindingLayoutAll }
  all = convertOptionsWithDefaults ExternalTextureBindingLayout defaultGPUExternalTextureBindingLayoutOptions provided

-- createPipelineLayout

foreign import createPipelineLayoutImpl :: forall entries. GPUDevice -> GPUPipelineLayoutDescriptor entries -> Effect (GPUPipelineLayout entries)

createPipelineLayout
  :: forall bindingLayouts
   . GPUDevice
  -> GPUPipelineLayoutDescriptor bindingLayouts
  -> Effect (GPUPipelineLayout bindingLayouts)
createPipelineLayout = createPipelineLayoutImpl
