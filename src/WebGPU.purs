module WebGPU
  ( Undefinable
  --
  , class ConvertOptionsWithDefaults
  , convertOptionsWithDefaults
  --
  , GPUPowerPreference
  , highPerformance
  , lowPower
  --
  , GPUTextureFormat
  , r8unorm
  , r8snorm
  , r8uint
  , r8sint
  , r16uint
  , r16sint
  , r16float
  , rg8unorm
  , rg8snorm
  , rg8uint
  , rg8sint
  , r32uint
  , r32sint
  , r32float
  , rg16uint
  , rg16sint
  , rg16float
  , rgba8unorm
  , rgba8unormSrgb
  , rgba8snorm
  , rgba8uint
  , rgba8sint
  , bgra8unorm
  , bgra8unormSrgb
  , rgb9e5ufloat
  , rgb10a2unorm
  , rg11b10ufloat
  , rg32uint
  , rg32sint
  , rg32float
  , rgba16uint
  , rgba16sint
  , rgba16float
  , rgba32uint
  , rgba32sint
  , rgba32float
  , stencil8
  , depth16unorm
  , depth24plus
  , depth24plusStencil8
  , depth32float
  , depth32floatStencil8
  , bc1RgbaUnorm
  , bc1RgbaUnormSrgb
  , bc2RgbaUnorm
  , bc2RgbaUnormSrgb
  , bc3RgbaUnorm
  , bc3RgbaUnormSrgb
  , bc4RUnorm
  , bc4RSnorm
  , bc5RgUnorm
  , bc5RgSnorm
  , bc6hRgbUfloat
  , bc6hRgbFloat
  , bc7RgbaUnorm
  , bc7RgbaUnormSrgb
  , etc2Rgb8unorm
  , etc2Rgb8unormSrgb
  , etc2Rgb8a1unorm
  , etc2Rgb8a1unormSrgb
  , etc2Rgba8unorm
  , etc2Rgba8unormSrgb
  , eacR11unorm
  , eacR11snorm
  , eacRg11unorm
  , eacRg11snorm
  , astc4x4Unorm
  , astc4x4UnormSrgb
  , astc5x4Unorm
  , astc5x4UnormSrgb
  , astc5x5Unorm
  , astc5x5UnormSrgb
  , astc6x5Unorm
  , astc6x5UnormSrgb
  , astc6x6Unorm
  , astc6x6UnormSrgb
  , astc8x5Unorm
  , astc8x5UnormSrgb
  , astc8x6Unorm
  , astc8x6UnormSrgb
  , astc8x8Unorm
  , astc8x8UnormSrgb
  , astc10x5Unorm
  , astc10x5UnormSrgb
  , astc10x6Unorm
  , astc10x6UnormSrgb
  , astc10x8Unorm
  , astc10x8UnormSrgb
  , astc10x10Unorm
  , astc10x10UnormSrgb
  , astc12x10Unorm
  , astc12x10UnormSrgb
  , astc12x12Unorm
  , astc12x12UnormSrgb
  --
  , GPU
  , gpu
  --
  , GPURequestAdapter
  , GPUAdapter
  , requestAdapter
  --
  , getPreferredCanvasFormat
  ) where

import Prelude

import Data.Maybe (Maybe(..))
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

r8unorm = GPUTextureFormat "r8unorm" :: GPUTextureFormat
r8snorm = GPUTextureFormat "r8snorm" :: GPUTextureFormat
r8uint = GPUTextureFormat "r8uint" :: GPUTextureFormat
r8sint = GPUTextureFormat "r8sint" :: GPUTextureFormat
r16uint = GPUTextureFormat "r16uint" :: GPUTextureFormat
r16sint = GPUTextureFormat "r16sint" :: GPUTextureFormat
r16float = GPUTextureFormat "r16float" :: GPUTextureFormat
rg8unorm = GPUTextureFormat "rg8unorm" :: GPUTextureFormat
rg8snorm = GPUTextureFormat "rg8snorm" :: GPUTextureFormat
rg8uint = GPUTextureFormat "rg8uint" :: GPUTextureFormat
rg8sint = GPUTextureFormat "rg8sint" :: GPUTextureFormat
r32uint = GPUTextureFormat "r32uint" :: GPUTextureFormat
r32sint = GPUTextureFormat "r32sint" :: GPUTextureFormat
r32float = GPUTextureFormat "r32float" :: GPUTextureFormat
rg16uint = GPUTextureFormat "rg16uint" :: GPUTextureFormat
rg16sint = GPUTextureFormat "rg16sint" :: GPUTextureFormat
rg16float = GPUTextureFormat "rg16float" :: GPUTextureFormat
rgba8unorm = GPUTextureFormat "rgba8unorm" :: GPUTextureFormat
rgba8unormSrgb = GPUTextureFormat "rgba8unorm-srgb" :: GPUTextureFormat
rgba8snorm = GPUTextureFormat "rgba8snorm" :: GPUTextureFormat
rgba8uint = GPUTextureFormat "rgba8uint" :: GPUTextureFormat
rgba8sint = GPUTextureFormat "rgba8sint" :: GPUTextureFormat
bgra8unorm = GPUTextureFormat "bgra8unorm" :: GPUTextureFormat
bgra8unormSrgb = GPUTextureFormat "bgra8unorm-srgb" :: GPUTextureFormat
rgb9e5ufloat = GPUTextureFormat "rgb9e5ufloat" :: GPUTextureFormat
rgb10a2unorm = GPUTextureFormat "rgb10a2unorm" :: GPUTextureFormat
rg11b10ufloat = GPUTextureFormat "rg11b10ufloat" :: GPUTextureFormat
rg32uint = GPUTextureFormat "rg32uint" :: GPUTextureFormat
rg32sint = GPUTextureFormat "rg32sint" :: GPUTextureFormat
rg32float = GPUTextureFormat "rg32float" :: GPUTextureFormat
rgba16uint = GPUTextureFormat "rgba16uint" :: GPUTextureFormat
rgba16sint = GPUTextureFormat "rgba16sint" :: GPUTextureFormat
rgba16float = GPUTextureFormat "rgba16float" :: GPUTextureFormat
rgba32uint = GPUTextureFormat "rgba32uint" :: GPUTextureFormat
rgba32sint = GPUTextureFormat "rgba32sint" :: GPUTextureFormat
rgba32float = GPUTextureFormat "rgba32float" :: GPUTextureFormat
stencil8 = GPUTextureFormat "stencil8" :: GPUTextureFormat
depth16unorm = GPUTextureFormat "depth16unorm" :: GPUTextureFormat
depth24plus = GPUTextureFormat "depth24plus" :: GPUTextureFormat
depth24plusStencil8 = GPUTextureFormat "depth24plus-stencil8" :: GPUTextureFormat
depth32float = GPUTextureFormat "depth32float" :: GPUTextureFormat
depth32floatStencil8 = GPUTextureFormat "depth32float-stencil8" :: GPUTextureFormat
bc1RgbaUnorm = GPUTextureFormat "bc1-rgba-unorm" :: GPUTextureFormat
bc1RgbaUnormSrgb = GPUTextureFormat "bc1-rgba-unorm-srgb" :: GPUTextureFormat
bc2RgbaUnorm = GPUTextureFormat "bc2-rgba-unorm" :: GPUTextureFormat
bc2RgbaUnormSrgb = GPUTextureFormat "bc2-rgba-unorm-srgb" :: GPUTextureFormat
bc3RgbaUnorm = GPUTextureFormat "bc3-rgba-unorm" :: GPUTextureFormat
bc3RgbaUnormSrgb = GPUTextureFormat "bc3-rgba-unorm-srgb" :: GPUTextureFormat
bc4RUnorm = GPUTextureFormat "bc4-r-unorm" :: GPUTextureFormat
bc4RSnorm = GPUTextureFormat "bc4-r-snorm" :: GPUTextureFormat
bc5RgUnorm = GPUTextureFormat "bc5-rg-unorm" :: GPUTextureFormat
bc5RgSnorm = GPUTextureFormat "bc5-rg-snorm" :: GPUTextureFormat
bc6hRgbUfloat = GPUTextureFormat "bc6h-rgb-ufloat" :: GPUTextureFormat
bc6hRgbFloat = GPUTextureFormat "bc6h-rgb-float" :: GPUTextureFormat
bc7RgbaUnorm = GPUTextureFormat "bc7-rgba-unorm" :: GPUTextureFormat
bc7RgbaUnormSrgb = GPUTextureFormat "bc7-rgba-unorm-srgb" :: GPUTextureFormat
etc2Rgb8unorm = GPUTextureFormat "etc2-rgb8unorm" :: GPUTextureFormat
etc2Rgb8unormSrgb = GPUTextureFormat "etc2-rgb8unorm-srgb" :: GPUTextureFormat
etc2Rgb8a1unorm = GPUTextureFormat "etc2-rgb8a1unorm" :: GPUTextureFormat
etc2Rgb8a1unormSrgb = GPUTextureFormat "etc2-rgb8a1unorm-srgb" :: GPUTextureFormat
etc2Rgba8unorm = GPUTextureFormat "etc2-rgba8unorm" :: GPUTextureFormat
etc2Rgba8unormSrgb = GPUTextureFormat "etc2-rgba8unorm-srgb" :: GPUTextureFormat
eacR11unorm = GPUTextureFormat "eac-r11unorm" :: GPUTextureFormat
eacR11snorm = GPUTextureFormat "eac-r11snorm" :: GPUTextureFormat
eacRg11unorm = GPUTextureFormat "eac-rg11unorm" :: GPUTextureFormat
eacRg11snorm = GPUTextureFormat "eac-rg11snorm" :: GPUTextureFormat
astc4x4Unorm = GPUTextureFormat "astc-4x4-unorm" :: GPUTextureFormat
astc4x4UnormSrgb = GPUTextureFormat "astc-4x4-unorm-srgb" :: GPUTextureFormat
astc5x4Unorm = GPUTextureFormat "astc-5x4-unorm" :: GPUTextureFormat
astc5x4UnormSrgb = GPUTextureFormat "astc-5x4-unorm-srgb" :: GPUTextureFormat
astc5x5Unorm = GPUTextureFormat "astc-5x5-unorm" :: GPUTextureFormat
astc5x5UnormSrgb = GPUTextureFormat "astc-5x5-unorm-srgb" :: GPUTextureFormat
astc6x5Unorm = GPUTextureFormat "astc-6x5-unorm" :: GPUTextureFormat
astc6x5UnormSrgb = GPUTextureFormat "astc-6x5-unorm-srgb" :: GPUTextureFormat
astc6x6Unorm = GPUTextureFormat "astc-6x6-unorm" :: GPUTextureFormat
astc6x6UnormSrgb = GPUTextureFormat "astc-6x6-unorm-srgb" :: GPUTextureFormat
astc8x5Unorm = GPUTextureFormat "astc-8x5-unorm" :: GPUTextureFormat
astc8x5UnormSrgb = GPUTextureFormat "astc-8x5-unorm-srgb" :: GPUTextureFormat
astc8x6Unorm = GPUTextureFormat "astc-8x6-unorm" :: GPUTextureFormat
astc8x6UnormSrgb = GPUTextureFormat "astc-8x6-unorm-srgb" :: GPUTextureFormat
astc8x8Unorm = GPUTextureFormat "astc-8x8-unorm" :: GPUTextureFormat
astc8x8UnormSrgb = GPUTextureFormat "astc-8x8-unorm-srgb" :: GPUTextureFormat
astc10x5Unorm = GPUTextureFormat "astc-10x5-unorm" :: GPUTextureFormat
astc10x5UnormSrgb = GPUTextureFormat "astc-10x5-unorm-srgb" :: GPUTextureFormat
astc10x6Unorm = GPUTextureFormat "astc-10x6-unorm" :: GPUTextureFormat
astc10x6UnormSrgb = GPUTextureFormat "astc-10x6-unorm-srgb" :: GPUTextureFormat
astc10x8Unorm = GPUTextureFormat "astc-10x8-unorm" :: GPUTextureFormat
astc10x8UnormSrgb = GPUTextureFormat "astc-10x8-unorm-srgb" :: GPUTextureFormat
astc10x10Unorm = GPUTextureFormat "astc-10x10-unorm" :: GPUTextureFormat
astc10x10UnormSrgb = GPUTextureFormat "astc-10x10-unorm-srgb" :: GPUTextureFormat
astc12x10Unorm = GPUTextureFormat "astc-12x10-unorm" :: GPUTextureFormat
astc12x10UnormSrgb = GPUTextureFormat "astc-12x10-unorm-srgb" :: GPUTextureFormat
astc12x12Unorm = GPUTextureFormat "astc-12x12-unorm" :: GPUTextureFormat
astc12x12UnormSrgb = GPUTextureFormat "astc-12x12-unorm-srgb" :: GPUTextureFormat

foreign import getPreferredCanvasFormatImpl :: GPU -> Effect GPUTextureFormat
getPreferredCanvasFormat :: GPU -> Effect GPUTextureFormat
getPreferredCanvasFormat= getPreferredCanvasFormatImpl