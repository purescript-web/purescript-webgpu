module Web.GPU.Device
  ( BufferBindingLayout
  , BufferDescriptor
  , ColorTargetState
  , ComputePipelineDescriptor
  , ExternalTextureBindingLayout
  , ExternalTextureDescriptor
  , GPUColorTargetState
  , RenderBundleEncoder
  , RenderPipelineDescriptor
  , SamplerBindingLayout
  , SamplerDescriptor
  , ShaderModuleDescriptor
  , StorageTextureBindingLayout
  , TextureBindingLayout
  , TextureDescriptor
  , VertexBufferLayout
  , bindGroupLayoutEntryForBuffer
  , bindGroupLayoutEntryForExternalTexture
  , bindGroupLayoutEntryForSampler
  , bindGroupLayoutEntryForStorageTexture
  , bindGroupLayoutEntryForTexture
  , colorTargetState
  , createBindGroup
  , createBindGroupLayout
  , createBuffer
  , createCommandEncoder
  , createComputePipeline
  , createComputePipelineAsnyc
  , createPipelineLayout
  , createQuerySet
  , createRenderBundleEncoder
  , createRenderPipeline
  , createRenderPipelineAsync
  , createSampler
  , createShaderModule
  , createTexture
  , destroy
  , features
  , importExternalTexture
  , limits
  , queue
  , vertexBufferLayout
  )
  where

import Prelude

import Data.Reflectable (class Reflectable, reflectType)
import Data.Set as Set
import Effect (Effect)
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Int (class Compare)
import Prim.Ordering (LT)
import Type.Proxy (Proxy)
import Web.GPU.GPUAddressMode (GPUAddressMode)
import Web.GPU.GPUBlendFactor (GPUBlendFactor)
import Web.GPU.GPUBlendOperation (GPUBlendOperation)
import Web.GPU.GPUBufferBindingType (GPUBufferBindingType)
import Web.GPU.GPUBufferUsage (GPUBufferUsage)
import Web.GPU.GPUColorWrite (GPUColorWrite)
import Web.GPU.GPUCompareFunction (GPUCompareFunction)
import Web.GPU.GPUCullMode (GPUCullMode)
import Web.GPU.GPUExtent3D (class AsGPUExtent3D, asGPUExtent3D)
import Web.GPU.GPUFeatureName (GPUFeatureName)
import Web.GPU.GPUFilterMode (GPUFilterMode)
import Web.GPU.GPUFrontFace (GPUFrontFace)
import Web.GPU.GPUIndexFormat (GPUIndexFormat)
import Web.GPU.GPUMipmapFilterMode (GPUMipmapFilterMode)
import Web.GPU.GPUPrimitiveTopology (GPUPrimitiveTopology)
import Web.GPU.GPUQueryType (GPUQueryType)
import Web.GPU.GPUSamplerBindingType (GPUSamplerBindingType)
import Web.GPU.GPUShaderStage (GPUShaderStage)
import Web.GPU.GPUStencilOperation (GPUStencilOperation)
import Web.GPU.GPUStorageTextureAccess (GPUStorageTextureAccess)
import Web.GPU.GPUSupportedLimits (GPUSupportedLimits)
import Web.GPU.GPUTextureDimension (GPUTextureDimension)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.GPUTextureSampleType (GPUTextureSampleType)
import Web.GPU.GPUTextureUsage (GPUTextureUsage)
import Web.GPU.GPUTextureViewDimension (GPUTextureViewDimension)
import Web.GPU.GPUVertexFormat (GPUVertexFormat)
import Web.GPU.GPUVertexStepMode (GPUVertexStepMode)
import Web.GPU.Internal.ConvertibleOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Web.GPU.Internal.Types (GPUBindGroup, GPUBindGroupDescriptor, GPUBindGroupLayout, GPUBindGroupLayoutDescriptor, GPUBindGroupLayoutEntry, GPUBuffer, GPUBufferBindingLayout, GPUCommandEncoder, GPUComputePipeline, GPUDevice, GPUExternalTexture, GPUExternalTextureBindingLayout, GPUPipelineLayout, GPUPipelineLayoutDescriptor, GPUQuerySet, GPUQueue, GPURenderPipeline, GPUSampler, GPUSamplerBindingLayout, GPUShaderModule, GPUShaderModuleCompilationHint, GPUStorageTextureBindingLayout, GPUTexture, GPUTextureBindingLayout)
import Web.GPU.Internal.Undefinable (Undefinable, defined, undefined)
import Web.GPU.Internal.Unsigned (GPUIndex32, GPUIntegerCoordinate, GPUSampleMask, GPUSize32, GPUSize64, GPUStencilValue, UnsignedShort, GPUDepthBias)
import Web.GPU.PredefinedColorSpace (PredefinedColorSpace)
import Web.HTML (HTMLVideoElement)
import Web.Promise (Promise)

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

data TextureDescriptor = TextureDescriptor

instance AsGPUExtent3D gpuExtent3DProvided gpuExtent3D => ConvertOption TextureDescriptor "size" gpuExtent3DProvided gpuExtent3D where
  convertOption _ _ = asGPUExtent3D

instance ConvertOption TextureDescriptor "mipLevelCount" GPUIntegerCoordinate (Undefinable GPUIntegerCoordinate) where
  convertOption _ _ = defined

instance ConvertOption TextureDescriptor "sampleCount" GPUSize32 (Undefinable GPUSize32) where
  convertOption _ _ = defined

instance ConvertOption TextureDescriptor "dimension" GPUTextureDimension (Undefinable GPUTextureDimension) where
  convertOption _ _ = defined

instance ConvertOption TextureDescriptor "viewFormats" (Array GPUTextureFormat) (Undefinable (Array GPUTextureFormat)) where
  convertOption _ _ = defined

instance ConvertOption TextureDescriptor "label" String (Undefinable String) where
  convertOption _ _ = defined

foreign import createTextureImpl :: forall gpuExtent3D. GPUDevice -> { | GPUTextureDescriptor gpuExtent3D } -> Effect GPUTexture

createTexture
  :: forall provided gpuExtent3D
   . ConvertOptionsWithDefaults TextureDescriptor { | GPUTextureDescriptorOptional } { | provided } { | GPUTextureDescriptor gpuExtent3D }
  => GPUDevice
  -> { | provided }
  -> Effect GPUTexture
createTexture gpuDevice provided = createTextureImpl gpuDevice all
  where
  all :: { | GPUTextureDescriptor gpuExtent3D }
  all = convertOptionsWithDefaults
    TextureDescriptor
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

-- createBindGroup

foreign import createBindGroupImpl :: forall layoutEntries entries. GPUDevice -> GPUBindGroupDescriptor layoutEntries entries -> Effect (GPUBindGroup layoutEntries entries)

createBindGroup :: forall layoutEntries entries. GPUDevice -> GPUBindGroupDescriptor layoutEntries entries -> Effect (GPUBindGroup layoutEntries entries)
createBindGroup = createBindGroupImpl

-- createShaderModule

type GPUShaderModuleDescriptorOptional =
  ( sourceMap :: Undefinable Foreign
  , hints :: Undefinable (Object GPUShaderModuleCompilationHint)
  )

type GPUShaderModuleDescriptor =
  ( code :: String
  | GPUShaderModuleDescriptorOptional
  )

defaultGPUShaderModuleDescriptorOptions :: { | GPUShaderModuleDescriptorOptional }
defaultGPUShaderModuleDescriptorOptions =
  { sourceMap: undefined
  , hints: undefined
  }

data ShaderModuleDescriptor = ShaderModuleDescriptor

instance ConvertOption ShaderModuleDescriptor "sourceMap" Foreign (Undefinable Foreign) where
  convertOption _ _ = defined

instance ConvertOption ShaderModuleDescriptor "hints" (Object GPUShaderModuleCompilationHint) (Undefinable (Object GPUShaderModuleCompilationHint)) where
  convertOption _ _ = defined

foreign import createShaderModuleImpl :: GPUDevice -> { | GPUShaderModuleDescriptor } -> Effect GPUShaderModule

createShaderModule
  :: forall provided
   . ConvertOptionsWithDefaults ShaderModuleDescriptor { | GPUShaderModuleDescriptorOptional } { | provided } { | GPUShaderModuleDescriptor }
  => GPUDevice
  -> { | provided }
  -> Effect GPUShaderModule
createShaderModule gpuDevice provided = createShaderModuleImpl gpuDevice all
  where
  all :: { | GPUShaderModuleDescriptor }
  all = convertOptionsWithDefaults ShaderModuleDescriptor defaultGPUShaderModuleDescriptorOptions provided

-- createComputePipeline
type GPUProgrammableStageOptional =
  (constants :: Undefinable (Object Number))

type GPUProgrammableStage =
  ( module :: GPUShaderModule
  , entryPoint :: String
  | GPUProgrammableStageOptional
  )

defaultGPUProgrammableStageOptions :: { | GPUProgrammableStageOptional }
defaultGPUProgrammableStageOptions = { constants: undefined }

data ProgrammableStage = ProgrammableStage

instance ConvertOption ProgrammableStage "constants" (Object Number) (Undefinable (Object Number)) where
  convertOption _ _ = defined

type GPUComputePipelineDescriptorOptional :: forall k. Row k
type GPUComputePipelineDescriptorOptional = ()

type GPUComputePipelineDescriptor =
  ( compute :: { | GPUProgrammableStage }
  | GPUComputePipelineDescriptorOptional
  )

defaultGPUComputePipelineDescriptorOptions :: { | GPUComputePipelineDescriptorOptional }
defaultGPUComputePipelineDescriptorOptions = {}

data ComputePipelineDescriptor = ComputePipelineDescriptor

instance ConvertOptionsWithDefaults ProgrammableStage { | GPUProgrammableStageOptional } { | provided } { | GPUProgrammableStage } => ConvertOption ComputePipelineDescriptor "compute" { | provided } { | GPUProgrammableStage } where
  convertOption _ _ = convertOptionsWithDefaults ProgrammableStage defaultGPUProgrammableStageOptions

foreign import createComputePipelineImpl :: GPUDevice -> { | GPUComputePipelineDescriptor } -> Effect GPUComputePipeline

createComputePipeline
  :: forall provided
   . ConvertOptionsWithDefaults ComputePipelineDescriptor { | GPUComputePipelineDescriptorOptional } { | provided } { | GPUComputePipelineDescriptor }
  => GPUDevice
  -> { | provided }
  -> Effect GPUComputePipeline
createComputePipeline gpuDevice provided = createComputePipelineImpl gpuDevice all
  where
  all :: { | GPUComputePipelineDescriptor }
  all = convertOptionsWithDefaults ComputePipelineDescriptor defaultGPUComputePipelineDescriptorOptions provided

-- createRenderPipeline

-- createRenderPipeline

-------- vertexAttribute
type GPUVertexAttribute =
  { format :: GPUVertexFormat
  , offset :: GPUSize64
  , shaderLocation :: GPUIndex32
  }

------ vertexBufferLayout

type GPUVertexBufferLayoutOptional =
  (stepMode :: Undefinable GPUVertexStepMode)

type GPUVertexBufferLayout =
  ( arrayStride :: GPUSize64
  , attributes :: Array GPUVertexAttribute
  | GPUVertexBufferLayoutOptional
  )

defaultGPUVertexBufferLayoutOptions :: { | GPUVertexBufferLayoutOptional }
defaultGPUVertexBufferLayoutOptions = { stepMode: undefined }

data VertexBufferLayout = VertexBufferLayout

instance ConvertOption VertexBufferLayout "stepMode" GPUVertexStepMode (Undefinable GPUVertexStepMode) where
  convertOption _ _ = defined

vertexBufferLayout
  :: forall provided
   . ConvertOptionsWithDefaults VertexBufferLayout { | GPUVertexBufferLayoutOptional } { | provided } { | GPUVertexBufferLayout }
  => { | provided }
  -> { | GPUVertexBufferLayout }
vertexBufferLayout provided = all
  where
  all :: { | GPUVertexBufferLayout }
  all = convertOptionsWithDefaults VertexBufferLayout defaultGPUVertexBufferLayoutOptions provided

---- vertexState
type GPUVertexStateOptional :: forall k. Row k
type GPUVertexStateOptional = ()

type GPUVertexState =
  ( buffers :: Array { | GPUVertexBufferLayout }
  | GPUVertexStateOptional
  )

defaultGPUVertexStateOptions :: { | GPUVertexStateOptional }
defaultGPUVertexStateOptions = {}

data VertexState = VertexState

---- primitiveState
type GPUPrimitiveStateOptional =
  ( topology :: Undefinable GPUPrimitiveTopology
  , stripIndexFormat :: Undefinable GPUIndexFormat
  , frontFace :: Undefinable GPUFrontFace
  , cullMode :: Undefinable GPUCullMode
  , unclippedDepth :: Undefinable Boolean
  )

type GPUPrimitiveState = (| GPUPrimitiveStateOptional)

defaultGPUPrimitiveStateOptions :: { | GPUPrimitiveStateOptional }
defaultGPUPrimitiveStateOptions =
  { topology: undefined
  , stripIndexFormat: undefined
  , frontFace: undefined
  , cullMode: undefined
  , unclippedDepth: undefined
  }

data PrimitiveState = PrimitiveState

instance ConvertOption PrimitiveState "topology" GPUPrimitiveTopology (Undefinable GPUPrimitiveTopology) where
  convertOption _ _ = defined

instance ConvertOption PrimitiveState "stripIndexFormat" GPUIndexFormat (Undefinable GPUIndexFormat) where
  convertOption _ _ = defined

instance ConvertOption PrimitiveState "frontFace" GPUFrontFace (Undefinable GPUFrontFace) where
  convertOption _ _ = defined

instance ConvertOption PrimitiveState "cullMode" GPUCullMode (Undefinable GPUCullMode) where
  convertOption _ _ = defined

instance ConvertOption PrimitiveState "unclippedDepth" Boolean (Undefinable Boolean) where
  convertOption _ _ = defined

------ gpuStencilFaceState

type GPUStencilFaceStateOptional =
  ( compare :: Undefinable GPUCompareFunction
  , failOp :: Undefinable GPUStencilOperation
  , depthFailOp :: Undefinable GPUStencilOperation
  , passOp :: Undefinable GPUStencilOperation
  )

type GPUStencilFaceState = (| GPUStencilFaceStateOptional)

defaultGPUStencilFaceStateOptions :: { | GPUStencilFaceStateOptional }
defaultGPUStencilFaceStateOptions = { compare: undefined, failOp: undefined, depthFailOp: undefined, passOp: undefined }

data StencilFaceState = StencilFaceState

instance ConvertOption StencilFaceState "compare" GPUCompareFunction (Undefinable GPUCompareFunction) where
  convertOption _ _ = defined

instance ConvertOption StencilFaceState "failOp" GPUStencilOperation (Undefinable GPUStencilOperation) where
  convertOption _ _ = defined

instance ConvertOption StencilFaceState "depthFailOp" GPUStencilOperation (Undefinable GPUStencilOperation) where
  convertOption _ _ = defined

instance ConvertOption StencilFaceState "passOp" GPUStencilOperation (Undefinable GPUStencilOperation) where
  convertOption _ _ = defined

---- depthStencilState
type GPUDepthStencilStateOptional :: forall k1 k2. k1 -> k2 -> Row Type
type GPUDepthStencilStateOptional stencilFront stencilBack =
  ( depthWriteEnabled :: Undefinable Boolean
  , depthCompare :: Undefinable GPUCompareFunction
  , stencilFront :: Undefinable stencilFront
  , stencilBack :: Undefinable stencilBack
  , stencilReadMask :: Undefinable GPUStencilValue
  , stencilWriteMask :: Undefinable GPUStencilValue
  , depthBias :: Undefinable GPUDepthBias
  , depthBiasSlopeScale :: Undefinable Number
  , depthBiasClamp :: Undefinable Number
  )

type GPUDepthStencilState :: forall k1 k2. k1 -> k2 -> Row Type
type GPUDepthStencilState stencilFront stencilBack =
  ( format :: GPUTextureFormat
  | GPUDepthStencilStateOptional stencilFront stencilBack
  )

defaultGPUDepthStencilStateOptions :: forall stencilFront stencilBack. { | GPUDepthStencilStateOptional stencilFront stencilBack }
defaultGPUDepthStencilStateOptions =
  { depthWriteEnabled: undefined
  , depthCompare: undefined
  , stencilFront: undefined
  , stencilBack: undefined
  , stencilReadMask: undefined
  , stencilWriteMask: undefined
  , depthBias: undefined
  , depthBiasSlopeScale: undefined
  , depthBiasClamp: undefined

  }

data DepthStencilState = DepthStencilState

instance ConvertOption DepthStencilState "depthWriteEnabled" Boolean (Undefinable Boolean) where
  convertOption _ _ = defined

instance ConvertOption DepthStencilState "depthCompare" GPUCompareFunction (Undefinable GPUCompareFunction) where
  convertOption _ _ = defined

instance ConvertOptionsWithDefaults StencilFaceState { | GPUStencilFaceStateOptional } { | stencilFront } { | GPUStencilFaceState } => ConvertOption DepthStencilState "stencilFront" { | stencilFront } (Undefinable { | GPUStencilFaceState }) where
  convertOption _ _ provided = defined $ convertOptionsWithDefaults StencilFaceState defaultGPUStencilFaceStateOptions provided

instance ConvertOptionsWithDefaults StencilFaceState { | GPUStencilFaceStateOptional } { | stencilBack } { | GPUStencilFaceState } => ConvertOption DepthStencilState "stencilBack" { | stencilBack } (Undefinable { | GPUStencilFaceState }) where
  convertOption _ _ provided = defined $ convertOptionsWithDefaults StencilFaceState defaultGPUStencilFaceStateOptions provided

instance ConvertOption DepthStencilState "stencilReadMask" GPUStencilValue (Undefinable GPUStencilValue) where
  convertOption _ _ = defined

instance ConvertOption DepthStencilState "stencilWriteMask" GPUStencilValue (Undefinable GPUStencilValue) where
  convertOption _ _ = defined

instance ConvertOption DepthStencilState "depthBias" GPUDepthBias (Undefinable GPUDepthBias) where
  convertOption _ _ = defined

instance ConvertOption DepthStencilState "depthBiasSlopeScale" Number (Undefinable Number) where
  convertOption _ _ = defined

instance ConvertOption DepthStencilState "depthBiasClamp" Number (Undefinable Number) where
  convertOption _ _ = defined

---- multisampleState
type GPUMultisampleStateOptional =
  ( count :: Undefinable GPUSize32
  , mask :: Undefinable GPUSampleMask
  , alphaToCoverageEnabled :: Undefinable Boolean
  )

type GPUMultisampleState =
  ( | GPUMultisampleStateOptional
  )

defaultGPUMultisampleStateOptions :: { | GPUMultisampleStateOptional }
defaultGPUMultisampleStateOptions = { count: undefined, mask: undefined, alphaToCoverageEnabled: undefined }

data MultisampleState = MultisampleState

instance ConvertOption MultisampleState "count" GPUSize32 (Undefinable GPUSize32) where
  convertOption _ _ = defined

instance ConvertOption MultisampleState "mask" GPUSampleMask (Undefinable GPUSampleMask) where
  convertOption _ _ = defined

instance ConvertOption MultisampleState "alphaToCoverageEnabled" Boolean (Undefinable Boolean) where
  convertOption _ _ = defined

------ GPUBlendComponent
type GPUBlendComponentOptional =
  ( operation :: Undefinable GPUBlendOperation
  , srcFactor :: Undefinable GPUBlendFactor
  , dstFactor :: Undefinable GPUBlendFactor
  )

type GPUBlendComponent = (| GPUBlendComponentOptional)

defaultGPUBlendComponentOptions :: { | GPUBlendComponentOptional }
defaultGPUBlendComponentOptions = { operation: undefined, srcFactor: undefined, dstFactor: undefined }

data BlendComponent = BlendComponent

instance ConvertOption BlendComponent "operation" GPUBlendOperation (Undefinable GPUBlendOperation) where
  convertOption _ _ = defined

instance ConvertOption BlendComponent "srcFactor" GPUBlendFactor (Undefinable GPUBlendFactor) where
  convertOption _ _ = defined

instance ConvertOption BlendComponent "dstFactor" GPUBlendFactor (Undefinable GPUBlendFactor) where
  convertOption _ _ = defined

------ colorTargetState

type GPUBlendState gpuBlendColor gpuBlendAlpha =
  ( color :: { | gpuBlendColor }
  , alpha :: { | gpuBlendAlpha }
  )

instance ConvertOptionsWithDefaults BlendComponent { | GPUBlendComponentOptional } { | gpuBlendColor } { | GPUBlendComponent } => ConvertOption BlendComponent "color" { | gpuBlendColor } { | GPUBlendComponent } where
  convertOption _ _ provided = convertOptionsWithDefaults BlendComponent defaultGPUBlendComponentOptions provided

instance ConvertOptionsWithDefaults BlendComponent { | GPUBlendComponentOptional } { | gpuBlendAlpha } { | GPUBlendComponent } => ConvertOption BlendComponent "alpha" { | gpuBlendAlpha } { | GPUBlendComponent } where
  convertOption _ _ provided = convertOptionsWithDefaults BlendComponent defaultGPUBlendComponentOptions provided

data BlendState = BlendState

type GPUColorTargetStateOptional gpuBlendColor gpuBlendAlpha =
  ( blend :: Undefinable { | GPUBlendState gpuBlendColor gpuBlendAlpha }
  , writeMask :: Undefinable GPUColorWrite
  )

type GPUColorTargetState' gpuBlendColor gpuBlendAlpha =
  ( format :: GPUTextureFormat
  | GPUColorTargetStateOptional gpuBlendColor gpuBlendAlpha
  )

newtype GPUColorTargetState = GPUColorTargetState { | GPUColorTargetState' GPUBlendComponent GPUBlendComponent }

defaultGPUColorTargetStateOptions :: forall gpuBlendColor gpuBlendAlpha. { | GPUColorTargetStateOptional gpuBlendColor gpuBlendAlpha }
defaultGPUColorTargetStateOptions = { blend: undefined, writeMask: undefined }

data ColorTargetState = ColorTargetState

instance
  ( ConvertOptionsWithDefaults BlendState {} { | blendState } { | GPUBlendState GPUBlendComponent GPUBlendComponent }
  ) =>
  ConvertOption ColorTargetState "blend" { | blendState } (Undefinable { | GPUBlendState GPUBlendComponent GPUBlendComponent }) where
  convertOption _ _ provided = defined $ convertOptionsWithDefaults BlendState {} provided

instance ConvertOption ColorTargetState "writeMask" GPUColorWrite (Undefinable GPUColorWrite) where
  convertOption _ _ = defined

colorTargetState
  :: forall provided gpuBlendColor gpuBlendAlpha
   . ConvertOptionsWithDefaults ColorTargetState { | GPUColorTargetStateOptional gpuBlendColor gpuBlendAlpha } { | provided } { | GPUColorTargetState' GPUBlendComponent GPUBlendComponent }
  => { | provided }
  -> GPUColorTargetState
colorTargetState provided = GPUColorTargetState all
  where
  all :: { | GPUColorTargetState' GPUBlendComponent GPUBlendComponent }
  all = convertOptionsWithDefaults ColorTargetState defaultGPUColorTargetStateOptions provided

---- fragmentState

type GPUFragmentStateOptional :: forall k. Row k
type GPUFragmentStateOptional = ()

type GPUFragmentState =
  ( targets :: Array GPUColorTargetState
  | GPUFragmentStateOptional
  )

defaultGPUFragmentStateOptions :: { | GPUFragmentStateOptional }
defaultGPUFragmentStateOptions = {}

data FragmentState = FragmentState

--
type GPURenderPipelineDescriptorOptional :: forall k1 k2. k1 -> k2 -> Row Type
type GPURenderPipelineDescriptorOptional stencilFront stencilBack =
  ( primitive :: Undefinable { | GPUPrimitiveState }
  , depthStencil :: Undefinable { | GPUDepthStencilState stencilFront stencilBack }
  , multisample :: Undefinable { | GPUMultisampleState }
  , fragment :: Undefinable { | GPUFragmentState }
  )

type GPURenderPipelineDescriptor :: forall k1 k2. k1 -> k2 -> Row Type
type GPURenderPipelineDescriptor stencilFront stencilBack =
  ( vertex :: { | GPUVertexState }
  | GPURenderPipelineDescriptorOptional stencilFront stencilBack
  )

defaultGPURenderPipelineDescriptorOptions :: forall stencilFront stencilBack. { | GPURenderPipelineDescriptorOptional stencilFront stencilBack }
defaultGPURenderPipelineDescriptorOptions = { primitive: undefined, depthStencil: undefined, multisample: undefined, fragment: undefined }

data RenderPipelineDescriptor = RenderPipelineDescriptor

instance ConvertOptionsWithDefaults PrimitiveState { | GPUPrimitiveStateOptional } { | provided } { | GPUPrimitiveState } => ConvertOption RenderPipelineDescriptor "primitive" { | provided } (Undefinable { | GPUPrimitiveState }) where
  convertOption _ _ provided = defined $ convertOptionsWithDefaults PrimitiveState defaultGPUPrimitiveStateOptions provided

instance ConvertOptionsWithDefaults DepthStencilState { | GPUDepthStencilStateOptional stencilFront stencilBack } { | provided } { | GPUDepthStencilState stencilFront stencilBack } => ConvertOption RenderPipelineDescriptor "depthStencil" { | provided } (Undefinable { | GPUDepthStencilState stencilFront stencilBack }) where
  convertOption _ _ provided = defined $ convertOptionsWithDefaults DepthStencilState defaultGPUDepthStencilStateOptions provided

instance ConvertOptionsWithDefaults MultisampleState { | GPUMultisampleStateOptional } { | provided } { | GPUMultisampleState } => ConvertOption RenderPipelineDescriptor "multisample" { | provided } (Undefinable { | GPUMultisampleState }) where
  convertOption _ _ provided = defined $ convertOptionsWithDefaults MultisampleState defaultGPUMultisampleStateOptions provided

instance ConvertOptionsWithDefaults FragmentState { | GPUFragmentStateOptional } { | provided } { | GPUFragmentState } => ConvertOption RenderPipelineDescriptor "fragment" { | provided } (Undefinable { | GPUFragmentState }) where
  convertOption _ _ provided = defined $ convertOptionsWithDefaults FragmentState defaultGPUFragmentStateOptions provided

instance ConvertOptionsWithDefaults VertexState { | GPUVertexStateOptional } { | provided } { | GPUVertexState } => ConvertOption RenderPipelineDescriptor "vertex" { | provided } { | GPUVertexState } where
  convertOption _ _ provided = convertOptionsWithDefaults VertexState defaultGPUVertexStateOptions provided

foreign import createRenderPipelineImpl :: forall stencilFront stencilBack. GPUDevice -> { | GPURenderPipelineDescriptor stencilFront stencilBack } -> Effect GPURenderPipeline

createRenderPipeline
  :: forall provided stencilFront stencilBack
   . ConvertOptionsWithDefaults RenderPipelineDescriptor { | GPURenderPipelineDescriptorOptional stencilFront stencilBack } { | provided } { | GPURenderPipelineDescriptor GPUStencilFaceState GPUStencilFaceState }
  => GPUDevice
  -> { | provided }
  -> Effect GPURenderPipeline
createRenderPipeline gpuDevice provided = createRenderPipelineImpl gpuDevice all
  where
  all :: { | GPURenderPipelineDescriptor GPUStencilFaceState GPUStencilFaceState }
  all = convertOptionsWithDefaults RenderPipelineDescriptor defaultGPURenderPipelineDescriptorOptions provided

-- createComputePipelineAsnyc
foreign import createComputePipelineAsyncImpl :: GPUDevice -> { | GPUComputePipelineDescriptor } -> Effect (Promise GPUComputePipeline)

createComputePipelineAsnyc
  :: forall provided
   . ConvertOptionsWithDefaults ComputePipelineDescriptor { | GPUComputePipelineDescriptorOptional } { | provided } { | GPUComputePipelineDescriptor }
  => GPUDevice
  -> { | provided }
  -> Effect (Promise GPUComputePipeline)
createComputePipelineAsnyc gpuDevice provided = createComputePipelineAsyncImpl gpuDevice all
  where
  all :: { | GPUComputePipelineDescriptor }
  all = convertOptionsWithDefaults ComputePipelineDescriptor defaultGPUComputePipelineDescriptorOptions provided

-- createRenderPipelineAsync

foreign import createRenderPipelineAsyncImpl :: forall stencilFront stencilBack. GPUDevice -> { | GPURenderPipelineDescriptor stencilFront stencilBack } -> Effect (Promise GPURenderPipeline)

createRenderPipelineAsync
  :: forall provided stencilFront stencilBack
   . ConvertOptionsWithDefaults RenderPipelineDescriptor { | GPURenderPipelineDescriptorOptional stencilFront stencilBack } { | provided } { | GPURenderPipelineDescriptor GPUStencilFaceState GPUStencilFaceState }
  => GPUDevice
  -> { | provided }
  -> Effect (Promise GPURenderPipeline)
createRenderPipelineAsync gpuDevice provided = createRenderPipelineAsyncImpl gpuDevice all
  where
  all :: { | GPURenderPipelineDescriptor GPUStencilFaceState GPUStencilFaceState }
  all = convertOptionsWithDefaults RenderPipelineDescriptor defaultGPURenderPipelineDescriptorOptions provided

-- createCommandEncoder
type GPUCommandEncoderDescriptor = {}

foreign import createCommandEncoderImpl :: GPUDevice -> GPUCommandEncoderDescriptor -> Effect GPUCommandEncoder

createCommandEncoder :: GPUDevice -> GPUCommandEncoderDescriptor -> Effect GPUCommandEncoder
createCommandEncoder = createCommandEncoderImpl

-- createRenderBundleEncoder
type GPURenderBundleEncoderOptional =
  ( depthReadOnly :: Undefinable Boolean
  , stencilReadOnly :: Undefinable Boolean
  )

type GPURenderBundleEncoder =
  (| GPURenderBundleEncoderOptional)

defaultGPURenderBundleEncoderOptions :: { | GPURenderBundleEncoderOptional }
defaultGPURenderBundleEncoderOptions =
  { depthReadOnly: undefined
  , stencilReadOnly: undefined
  }

data RenderBundleEncoder = RenderBundleEncoder

instance ConvertOption RenderBundleEncoder "depthReadOnly" Boolean (Undefinable Boolean) where
  convertOption _ _ = defined

instance ConvertOption RenderBundleEncoder "stencilReadOnly" Boolean (Undefinable Boolean) where
  convertOption _ _ = defined

foreign import createRenderBundleEncoderImpl :: GPUDevice -> { | GPURenderBundleEncoder } -> Effect GPUBuffer

createRenderBundleEncoder
  :: forall provided
   . ConvertOptionsWithDefaults RenderBundleEncoder { | GPURenderBundleEncoderOptional } { | provided } { | GPURenderBundleEncoder }
  => GPUDevice
  -> { | provided }
  -> Effect GPUBuffer
createRenderBundleEncoder gpuDevice provided = createRenderBundleEncoderImpl gpuDevice all
  where
  all :: { | GPURenderBundleEncoder }
  all = convertOptionsWithDefaults RenderBundleEncoder defaultGPURenderBundleEncoderOptions provided

-- createQuerySet
type GPUQuerySetDescriptor =
  { type :: GPUQueryType
  , count :: GPUSize32
  }

foreign import createQuerySetImpl :: GPUDevice -> GPUQuerySetDescriptor -> Effect GPUQuerySet

createQuerySet :: GPUDevice -> GPUQuerySetDescriptor -> Effect GPUQuerySet
createQuerySet = createQuerySetImpl
