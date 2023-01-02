module WebGPU
  ( GPU
  , GPUAdapter
  , GPUFeatureName
  , GPUPowerPreference
  , GPURequestAdapter
  , GPUTextureFormat
  , Undefinable
  , UnsignedLong
  , UnsignedLongLong
  , class ConvertOptionsWithDefaults
  , convertOptionsWithDefaults
  , features
  , fn'bgra8unormStorage
  , fn'depth32floatStencil8
  , fn'depthClipControl
  , fn'indirectFirstInstance
  , fn'rg11b10ufloatRenderable
  , fn'shaderF16
  , fn'textureCompressionAstc
  , fn'textureCompressionBc
  , fn'textureCompressionEtc2
  , fn'timestampQuery
  , getPreferredCanvasFormat
  , gpu
  , highPerformance
  , limits
  , lowPower
  , requestAdapter
  , tf'astc10x10Unorm
  , tf'astc10x10UnormSrgb
  , tf'astc10x5Unorm
  , tf'astc10x5UnormSrgb
  , tf'astc10x6Unorm
  , tf'astc10x6UnormSrgb
  , tf'astc10x8Unorm
  , tf'astc10x8UnormSrgb
  , tf'astc12x10Unorm
  , tf'astc12x10UnormSrgb
  , tf'astc12x12Unorm
  , tf'astc12x12UnormSrgb
  , tf'astc4x4Unorm
  , tf'astc4x4UnormSrgb
  , tf'astc5x4Unorm
  , tf'astc5x4UnormSrgb
  , tf'astc5x5Unorm
  , tf'astc5x5UnormSrgb
  , tf'astc6x5Unorm
  , tf'astc6x5UnormSrgb
  , tf'astc6x6Unorm
  , tf'astc6x6UnormSrgb
  , tf'astc8x5Unorm
  , tf'astc8x5UnormSrgb
  , tf'astc8x6Unorm
  , tf'astc8x6UnormSrgb
  , tf'astc8x8Unorm
  , tf'astc8x8UnormSrgb
  , tf'bc1RgbaUnorm
  , tf'bc1RgbaUnormSrgb
  , tf'bc2RgbaUnorm
  , tf'bc2RgbaUnormSrgb
  , tf'bc3RgbaUnorm
  , tf'bc3RgbaUnormSrgb
  , tf'bc4RSnorm
  , tf'bc4RUnorm
  , tf'bc5RgSnorm
  , tf'bc5RgUnorm
  , tf'bc6hRgbFloat
  , tf'bc6hRgbUfloat
  , tf'bc7RgbaUnorm
  , tf'bc7RgbaUnormSrgb
  , tf'bgra8unorm
  , tf'bgra8unormSrgb
  , tf'depth16unorm
  , tf'depth24plus
  , tf'depth24plusStencil8
  , tf'depth32float
  , tf'depth32floatStencil8
  , tf'eacR11snorm
  , tf'eacR11unorm
  , tf'eacRg11snorm
  , tf'eacRg11unorm
  , tf'etc2Rgb8a1unorm
  , tf'etc2Rgb8a1unormSrgb
  , tf'etc2Rgb8unorm
  , tf'etc2Rgb8unormSrgb
  , tf'etc2Rgba8unorm
  , tf'etc2Rgba8unormSrgb
  , tf'r16float
  , tf'r16sint
  , tf'r16uint
  , tf'r32float
  , tf'r32sint
  , tf'r32uint
  , tf'r8sint
  , tf'r8snorm
  , tf'r8uint
  , tf'r8unorm
  , tf'rg11b10ufloat
  , tf'rg16float
  , tf'rg16sint
  , tf'rg16uint
  , tf'rg32float
  , tf'rg32sint
  , tf'rg32uint
  , tf'rg8sint
  , tf'rg8snorm
  , tf'rg8uint
  , tf'rg8unorm
  , tf'rgb10a2unorm
  , tf'rgb9e5ufloat
  , tf'rgba16float
  , tf'rgba16sint
  , tf'rgba16uint
  , tf'rgba32float
  , tf'rgba32sint
  , tf'rgba32uint
  , tf'rgba8sint
  , tf'rgba8snorm
  , tf'rgba8uint
  , tf'rgba8unorm
  , tf'rgba8unormSrgb
  , tf'stencil8
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RowList
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML.Navigator (Navigator)
import Web.Promise (Promise)

-- Undefinable

data Undefinable :: forall k. k -> Type
data Undefinable a

foreign import undefinedImpl :: forall a. Undefinable a

undefined :: forall a. Undefinable a
undefined = undefinedImpl

defined :: forall a. a -> Undefinable a
defined = unsafeCoerce

-- int
type UnsignedLong = Int
type UnsignedLongLong = Int

-- convertible options

class ConvertOptionsWithDefaults t defaults provided all | t -> defaults all where
  convertOptionsWithDefaults :: t -> defaults -> provided -> all

instance convertOptionsWithDefaultsRecord ::
  ( ConvertOptions t { | provided } provided'
  , Defaults { | defaults } provided' { | all }
  ) =>
  ConvertOptionsWithDefaults t { | defaults } { | provided } { | all } where
  convertOptionsWithDefaults t def =
    defaults def <<< convertOptions t

class ConvertOptions t i o | t -> o where
  convertOptions :: t -> i -> o

class ConvertOption t (p :: Symbol) i o | t p -> o where
  convertOption :: t -> Proxy p -> i -> o

class ConvertRecordOptions t (rl :: RowList Type) i o | t rl -> o where
  convertRecordOptions :: t -> Proxy rl -> i -> o

instance convertRecordOptionsNil :: ConvertRecordOptions t RowList.Nil { | r } (Builder {} {}) where
  convertRecordOptions _ _ _ = identity

instance convertRecordOptionsCons ::
  ( ConvertRecordOptions t rest { | r } (Builder { | i } { | o' })
  , ConvertOption t sym a b
  , Row.Cons sym a r' r
  , Row.Cons sym b o' o
  , Row.Lacks sym o'
  , IsSymbol sym
  ) =>
  ConvertRecordOptions t (RowList.Cons sym a rest) { | r } (Builder { | i } { | o }) where
  convertRecordOptions t _ r =
    Builder.insert (Proxy :: _ sym) (convertOption t (Proxy :: _ sym) (Record.get (Proxy :: _ sym) r))
      <<< convertRecordOptions t (Proxy :: _ rest) r

instance convertOptionsRecord ::
  ( RowToList i rl
  , ConvertRecordOptions t rl { | i } (Builder {} { | o })
  ) =>
  ConvertOptions t { | i } { | o } where
  convertOptions t i = Builder.buildFromScratch $ convertRecordOptions t (Proxy :: _ rl) i

class Defaults defaults provided all | defaults provided -> all where
  defaults :: defaults -> provided -> all

instance defaultsRecord ::
  ( Row.Union provided defaults all'
  , Row.Nub all' all
  ) =>
  Defaults { | defaults } { | provided } { | all } where
  defaults = flip Record.merge

-----
-----
-----

-- gpu
data GPU

foreign import gpuImpl :: (GPU -> Maybe GPU) -> Maybe GPU -> Navigator -> Effect (Maybe GPU)

gpu :: Navigator -> Effect (Maybe GPU)
gpu = gpuImpl Just Nothing

data GPUAdapter

foreign import requestAdapterImpl :: (GPUAdapter -> Maybe GPUAdapter) -> Maybe GPUAdapter -> GPU -> { | GPURequestAdapterOptions } -> Effect (Promise (Maybe GPUAdapter))

newtype GPUPowerPreference = GPUPowerPreference String

derive instance Eq GPUPowerPreference
derive instance Ord GPUPowerPreference
derive newtype instance Show GPUPowerPreference

lowPower ∷ GPUPowerPreference
lowPower = GPUPowerPreference "low-power"

highPerformance ∷ GPUPowerPreference
highPerformance = GPUPowerPreference "high-performance"

---- requestAdapter

type GPURequestAdapterOptionsOptional =
  ( powerPreference :: Undefinable GPUPowerPreference
  , forceFallbackAdapter :: Undefinable Boolean
  )

type GPURequestAdapterOptions = (| GPURequestAdapterOptionsOptional)

defaultGPURequestAdapterOptions :: { | GPURequestAdapterOptionsOptional }
defaultGPURequestAdapterOptions =
  { powerPreference: undefined
  , forceFallbackAdapter: undefined
  }

data GPURequestAdapter = GPURequestAdapter

instance ConvertOption GPURequestAdapter "powerPreference" GPUPowerPreference (Undefinable GPUPowerPreference) where
  convertOption _ _ = defined

instance ConvertOption GPURequestAdapter "forceFallbackAdapter" Boolean (Undefinable Boolean) where
  convertOption _ _ = defined

requestAdapter
  :: forall provided
   . ConvertOptionsWithDefaults GPURequestAdapter { | GPURequestAdapterOptionsOptional } { | provided } { | GPURequestAdapterOptions }
  => GPU
  -> { | provided }
  -> Effect (Promise (Maybe GPUAdapter))
requestAdapter g provided = requestAdapterImpl Just Nothing g all
  where
  all :: { | GPURequestAdapterOptions }
  all = convertOptionsWithDefaults GPURequestAdapter defaultGPURequestAdapterOptions provided

---- getPreferredCanvasFormat
newtype GPUTextureFormat = GPUTextureFormat String

derive instance Eq GPUTextureFormat
derive instance Ord GPUTextureFormat
derive newtype instance Show GPUTextureFormat

tf'r8unorm = GPUTextureFormat "r8unorm" :: GPUTextureFormat
tf'r8snorm = GPUTextureFormat "r8snorm" :: GPUTextureFormat
tf'r8uint = GPUTextureFormat "r8uint" :: GPUTextureFormat
tf'r8sint = GPUTextureFormat "r8sint" :: GPUTextureFormat
tf'r16uint = GPUTextureFormat "r16uint" :: GPUTextureFormat
tf'r16sint = GPUTextureFormat "r16sint" :: GPUTextureFormat
tf'r16float = GPUTextureFormat "r16float" :: GPUTextureFormat
tf'rg8unorm = GPUTextureFormat "rg8unorm" :: GPUTextureFormat
tf'rg8snorm = GPUTextureFormat "rg8snorm" :: GPUTextureFormat
tf'rg8uint = GPUTextureFormat "rg8uint" :: GPUTextureFormat
tf'rg8sint = GPUTextureFormat "rg8sint" :: GPUTextureFormat
tf'r32uint = GPUTextureFormat "r32uint" :: GPUTextureFormat
tf'r32sint = GPUTextureFormat "r32sint" :: GPUTextureFormat
tf'r32float = GPUTextureFormat "r32float" :: GPUTextureFormat
tf'rg16uint = GPUTextureFormat "rg16uint" :: GPUTextureFormat
tf'rg16sint = GPUTextureFormat "rg16sint" :: GPUTextureFormat
tf'rg16float = GPUTextureFormat "rg16float" :: GPUTextureFormat
tf'rgba8unorm = GPUTextureFormat "rgba8unorm" :: GPUTextureFormat
tf'rgba8unormSrgb = GPUTextureFormat "rgba8unorm-srgb" :: GPUTextureFormat
tf'rgba8snorm = GPUTextureFormat "rgba8snorm" :: GPUTextureFormat
tf'rgba8uint = GPUTextureFormat "rgba8uint" :: GPUTextureFormat
tf'rgba8sint = GPUTextureFormat "rgba8sint" :: GPUTextureFormat
tf'bgra8unorm = GPUTextureFormat "bgra8unorm" :: GPUTextureFormat
tf'bgra8unormSrgb = GPUTextureFormat "bgra8unorm-srgb" :: GPUTextureFormat
tf'rgb9e5ufloat = GPUTextureFormat "rgb9e5ufloat" :: GPUTextureFormat
tf'rgb10a2unorm = GPUTextureFormat "rgb10a2unorm" :: GPUTextureFormat
tf'rg11b10ufloat = GPUTextureFormat "rg11b10ufloat" :: GPUTextureFormat
tf'rg32uint = GPUTextureFormat "rg32uint" :: GPUTextureFormat
tf'rg32sint = GPUTextureFormat "rg32sint" :: GPUTextureFormat
tf'rg32float = GPUTextureFormat "rg32float" :: GPUTextureFormat
tf'rgba16uint = GPUTextureFormat "rgba16uint" :: GPUTextureFormat
tf'rgba16sint = GPUTextureFormat "rgba16sint" :: GPUTextureFormat
tf'rgba16float = GPUTextureFormat "rgba16float" :: GPUTextureFormat
tf'rgba32uint = GPUTextureFormat "rgba32uint" :: GPUTextureFormat
tf'rgba32sint = GPUTextureFormat "rgba32sint" :: GPUTextureFormat
tf'rgba32float = GPUTextureFormat "rgba32float" :: GPUTextureFormat
tf'stencil8 = GPUTextureFormat "stencil8" :: GPUTextureFormat
tf'depth16unorm = GPUTextureFormat "depth16unorm" :: GPUTextureFormat
tf'depth24plus = GPUTextureFormat "depth24plus" :: GPUTextureFormat
tf'depth24plusStencil8 = GPUTextureFormat "depth24plus-stencil8" :: GPUTextureFormat
tf'depth32float = GPUTextureFormat "depth32float" :: GPUTextureFormat
tf'depth32floatStencil8 = GPUTextureFormat "depth32float-stencil8" :: GPUTextureFormat
tf'bc1RgbaUnorm = GPUTextureFormat "bc1-rgba-unorm" :: GPUTextureFormat
tf'bc1RgbaUnormSrgb = GPUTextureFormat "bc1-rgba-unorm-srgb" :: GPUTextureFormat
tf'bc2RgbaUnorm = GPUTextureFormat "bc2-rgba-unorm" :: GPUTextureFormat
tf'bc2RgbaUnormSrgb = GPUTextureFormat "bc2-rgba-unorm-srgb" :: GPUTextureFormat
tf'bc3RgbaUnorm = GPUTextureFormat "bc3-rgba-unorm" :: GPUTextureFormat
tf'bc3RgbaUnormSrgb = GPUTextureFormat "bc3-rgba-unorm-srgb" :: GPUTextureFormat
tf'bc4RUnorm = GPUTextureFormat "bc4-r-unorm" :: GPUTextureFormat
tf'bc4RSnorm = GPUTextureFormat "bc4-r-snorm" :: GPUTextureFormat
tf'bc5RgUnorm = GPUTextureFormat "bc5-rg-unorm" :: GPUTextureFormat
tf'bc5RgSnorm = GPUTextureFormat "bc5-rg-snorm" :: GPUTextureFormat
tf'bc6hRgbUfloat = GPUTextureFormat "bc6h-rgb-ufloat" :: GPUTextureFormat
tf'bc6hRgbFloat = GPUTextureFormat "bc6h-rgb-float" :: GPUTextureFormat
tf'bc7RgbaUnorm = GPUTextureFormat "bc7-rgba-unorm" :: GPUTextureFormat
tf'bc7RgbaUnormSrgb = GPUTextureFormat "bc7-rgba-unorm-srgb" :: GPUTextureFormat
tf'etc2Rgb8unorm = GPUTextureFormat "etc2-rgb8unorm" :: GPUTextureFormat
tf'etc2Rgb8unormSrgb = GPUTextureFormat "etc2-rgb8unorm-srgb" :: GPUTextureFormat
tf'etc2Rgb8a1unorm = GPUTextureFormat "etc2-rgb8a1unorm" :: GPUTextureFormat
tf'etc2Rgb8a1unormSrgb = GPUTextureFormat "etc2-rgb8a1unorm-srgb" :: GPUTextureFormat
tf'etc2Rgba8unorm = GPUTextureFormat "etc2-rgba8unorm" :: GPUTextureFormat
tf'etc2Rgba8unormSrgb = GPUTextureFormat "etc2-rgba8unorm-srgb" :: GPUTextureFormat
tf'eacR11unorm = GPUTextureFormat "eac-r11unorm" :: GPUTextureFormat
tf'eacR11snorm = GPUTextureFormat "eac-r11snorm" :: GPUTextureFormat
tf'eacRg11unorm = GPUTextureFormat "eac-rg11unorm" :: GPUTextureFormat
tf'eacRg11snorm = GPUTextureFormat "eac-rg11snorm" :: GPUTextureFormat
tf'astc4x4Unorm = GPUTextureFormat "astc-4x4-unorm" :: GPUTextureFormat
tf'astc4x4UnormSrgb = GPUTextureFormat "astc-4x4-unorm-srgb" :: GPUTextureFormat
tf'astc5x4Unorm = GPUTextureFormat "astc-5x4-unorm" :: GPUTextureFormat
tf'astc5x4UnormSrgb = GPUTextureFormat "astc-5x4-unorm-srgb" :: GPUTextureFormat
tf'astc5x5Unorm = GPUTextureFormat "astc-5x5-unorm" :: GPUTextureFormat
tf'astc5x5UnormSrgb = GPUTextureFormat "astc-5x5-unorm-srgb" :: GPUTextureFormat
tf'astc6x5Unorm = GPUTextureFormat "astc-6x5-unorm" :: GPUTextureFormat
tf'astc6x5UnormSrgb = GPUTextureFormat "astc-6x5-unorm-srgb" :: GPUTextureFormat
tf'astc6x6Unorm = GPUTextureFormat "astc-6x6-unorm" :: GPUTextureFormat
tf'astc6x6UnormSrgb = GPUTextureFormat "astc-6x6-unorm-srgb" :: GPUTextureFormat
tf'astc8x5Unorm = GPUTextureFormat "astc-8x5-unorm" :: GPUTextureFormat
tf'astc8x5UnormSrgb = GPUTextureFormat "astc-8x5-unorm-srgb" :: GPUTextureFormat
tf'astc8x6Unorm = GPUTextureFormat "astc-8x6-unorm" :: GPUTextureFormat
tf'astc8x6UnormSrgb = GPUTextureFormat "astc-8x6-unorm-srgb" :: GPUTextureFormat
tf'astc8x8Unorm = GPUTextureFormat "astc-8x8-unorm" :: GPUTextureFormat
tf'astc8x8UnormSrgb = GPUTextureFormat "astc-8x8-unorm-srgb" :: GPUTextureFormat
tf'astc10x5Unorm = GPUTextureFormat "astc-10x5-unorm" :: GPUTextureFormat
tf'astc10x5UnormSrgb = GPUTextureFormat "astc-10x5-unorm-srgb" :: GPUTextureFormat
tf'astc10x6Unorm = GPUTextureFormat "astc-10x6-unorm" :: GPUTextureFormat
tf'astc10x6UnormSrgb = GPUTextureFormat "astc-10x6-unorm-srgb" :: GPUTextureFormat
tf'astc10x8Unorm = GPUTextureFormat "astc-10x8-unorm" :: GPUTextureFormat
tf'astc10x8UnormSrgb = GPUTextureFormat "astc-10x8-unorm-srgb" :: GPUTextureFormat
tf'astc10x10Unorm = GPUTextureFormat "astc-10x10-unorm" :: GPUTextureFormat
tf'astc10x10UnormSrgb = GPUTextureFormat "astc-10x10-unorm-srgb" :: GPUTextureFormat
tf'astc12x10Unorm = GPUTextureFormat "astc-12x10-unorm" :: GPUTextureFormat
tf'astc12x10UnormSrgb = GPUTextureFormat "astc-12x10-unorm-srgb" :: GPUTextureFormat
tf'astc12x12Unorm = GPUTextureFormat "astc-12x12-unorm" :: GPUTextureFormat
tf'astc12x12UnormSrgb = GPUTextureFormat "astc-12x12-unorm-srgb" :: GPUTextureFormat

foreign import getPreferredCanvasFormatImpl :: GPU -> Effect GPUTextureFormat

getPreferredCanvasFormat :: GPU -> Effect GPUTextureFormat
getPreferredCanvasFormat = getPreferredCanvasFormatImpl

-- GPUAdapter

newtype GPUFeatureName = GPUFeatureName String

derive instance Eq GPUFeatureName
derive instance Ord GPUFeatureName
derive newtype instance Show GPUFeatureName
fn'depthClipControl = GPUFeatureName "depth-clip-control" :: GPUFeatureName
fn'depth32floatStencil8 = GPUFeatureName "depth32float-stencil8" :: GPUFeatureName
fn'textureCompressionBc = GPUFeatureName "texture-compression-bc" :: GPUFeatureName
fn'textureCompressionEtc2 = GPUFeatureName "texture-compression-etc2" :: GPUFeatureName
fn'textureCompressionAstc = GPUFeatureName "texture-compression-astc" :: GPUFeatureName
fn'timestampQuery = GPUFeatureName "timestamp-query" :: GPUFeatureName
fn'indirectFirstInstance = GPUFeatureName "indirect-first-instance" :: GPUFeatureName
fn'shaderF16 = GPUFeatureName "shader-f16" :: GPUFeatureName
fn'bgra8unormStorage = GPUFeatureName "bgra8unorm-storage" :: GPUFeatureName
fn'rg11b10ufloatRenderable = GPUFeatureName "rg11b10ufloat-renderable" :: GPUFeatureName

---- features
foreign import featuresImpl :: (GPUFeatureName -> Set.Set GPUFeatureName -> Set.Set GPUFeatureName) -> Set.Set GPUFeatureName -> GPUAdapter -> Effect (Set.Set GPUFeatureName)

features :: GPUAdapter -> Effect (Set.Set GPUFeatureName)
features = featuresImpl Set.insert Set.empty

-- limits
type GPUSupportedLimits =
  { maxTextureDimension1D :: UnsignedLong
  , maxTextureDimension2D :: UnsignedLong
  , maxTextureDimension3D :: UnsignedLong
  , maxTextureArrayLayers :: UnsignedLong
  , maxBindGroups :: UnsignedLong
  , maxBindingsPerBindGroup :: UnsignedLong
  , maxDynamicUniformBuffersPerPipelineLayout :: UnsignedLong
  , maxDynamicStorageBuffersPerPipelineLayout :: UnsignedLong
  , maxSampledTexturesPerShaderStage :: UnsignedLong
  , maxSamplersPerShaderStage :: UnsignedLong
  , maxStorageBuffersPerShaderStage :: UnsignedLong
  , maxStorageTexturesPerShaderStage :: UnsignedLong
  , maxUniformBuffersPerShaderStage :: UnsignedLong
  , maxUniformBufferBindingSize :: UnsignedLongLong
  , maxStorageBufferBindingSize :: UnsignedLongLong
  , minUniformBufferOffsetAlignment :: UnsignedLong
  , minStorageBufferOffsetAlignment :: UnsignedLong
  , maxVertexBuffers :: UnsignedLong
  , maxBufferSize :: UnsignedLongLong
  , maxVertexAttributes :: UnsignedLong
  , maxVertexBufferArrayStride :: UnsignedLong
  , maxInterStageShaderComponents :: UnsignedLong
  , maxInterStageShaderVariables :: UnsignedLong
  , maxColorAttachments :: UnsignedLong
  , maxColorAttachmentBytesPerSample :: UnsignedLong
  , maxComputeWorkgroupStorageSize :: UnsignedLong
  , maxComputeInvocationsPerWorkgroup :: UnsignedLong
  , maxComputeWorkgroupSizeX :: UnsignedLong
  , maxComputeWorkgroupSizeY :: UnsignedLong
  , maxComputeWorkgroupSizeZ :: UnsignedLong
  , maxComputeWorkgroupsPerDimension :: UnsignedLong
  }

foreign import limitsImpl :: GPUAdapter -> Effect GPUSupportedLimits

limits :: GPUAdapter -> Effect GPUSupportedLimits
limits = limitsImpl