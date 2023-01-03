module Web.GPU.GPU
  ( requestAdapter
  , getPreferredCanvasFormat
  , GPURequestAdapter
  ) where


import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.GPU.GPUPowerPreference (GPUPowerPreference)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.Internal.ConvertibleOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Web.GPU.Internal.Types (GPU, GPUAdapter)
import Web.GPU.Internal.Undefinable (Undefinable, undefined, defined)
import Web.Promise (Promise)

-- requestAdapter

foreign import requestAdapterImpl :: (GPUAdapter -> Maybe GPUAdapter) -> Maybe GPUAdapter -> GPU -> { | GPURequestAdapterOptions } -> Effect (Promise (Maybe GPUAdapter))

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

-- getPreferredCanvasFormat

foreign import getPreferredCanvasFormatImpl :: GPU -> Effect GPUTextureFormat

getPreferredCanvasFormat :: GPU -> Effect GPUTextureFormat
getPreferredCanvasFormat = getPreferredCanvasFormatImpl
