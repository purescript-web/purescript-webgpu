module Web.GPU.GPUAdapter
  ( GPUAdapterInfo
  , GPURequestDevice
  , features
  , limits
  , isFallbackAdapter
  , requestAdapterInfo
  , requestDevice
  ) where

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Web.GPU.Internal.Types (GPUAdapter, GPUDevice)
import Web.GPU.GPUFeatureName (GPUFeatureName)
import Web.GPU.GPUSupportedLimits (GPUSupportedLimits, UndefinableGPUSupportedLimits, defaultGPUSupportedLimits)
import Web.GPU.Internal.ConvertibleOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Web.GPU.Internal.Undefinable (Undefinable, undefined, defined)
import Web.Promise (Promise)
import Web.GPU.UnmaskHint (UnmaskHint)

-- features
foreign import featuresImpl :: (GPUFeatureName -> Set.Set GPUFeatureName -> Set.Set GPUFeatureName) -> Set.Set GPUFeatureName -> GPUAdapter -> Effect (Set.Set GPUFeatureName)

features :: GPUAdapter -> Effect (Set.Set GPUFeatureName)
features = featuresImpl Set.insert Set.empty

foreign import limitsImpl :: GPUAdapter -> Effect { | GPUSupportedLimits }

limits :: GPUAdapter -> Effect { | GPUSupportedLimits }
limits = limitsImpl

foreign import isFallbackAdapterImpl :: GPUAdapter -> Effect Boolean

isFallbackAdapter :: GPUAdapter -> Effect Boolean
isFallbackAdapter = isFallbackAdapterImpl

-- requestDevice

foreign import requestDeviceImpl :: (GPUDevice -> Maybe GPUDevice) -> Maybe GPUDevice -> GPUAdapter -> { | GPURequestDeviceOptions } -> Effect (Promise (Maybe GPUDevice))

type GPUQueueDescriptorOptional =
  ( label :: Undefinable String
  )

type GPUQueueDescriptor = (| GPUQueueDescriptorOptional)

defaultGPUQueueDescriptor :: { | GPUQueueDescriptor }
defaultGPUQueueDescriptor = { label: undefined }

type GPURequestDeviceOptionsOptional =
  ( requiredFeatures :: Undefinable (Array GPUFeatureName)
  , requiredLimits :: Undefinable { | UndefinableGPUSupportedLimits }
  , defaultQueue :: Undefinable { | GPUQueueDescriptor }
  )

type GPURequestDeviceOptions = (| GPURequestDeviceOptionsOptional)

defaultGPURequestDeviceOptions :: { | GPURequestDeviceOptionsOptional }
defaultGPURequestDeviceOptions =
  { requiredFeatures: undefined
  , requiredLimits: undefined
  , defaultQueue: undefined
  }

data GPURequestDevice = GPURequestDevice

instance ConvertOption GPURequestDevice "requiredFeatures" (Array GPUFeatureName) (Undefinable (Array GPUFeatureName)) where
  convertOption _ _ = defined

instance ConvertOptionsWithDefaults GPURequestDevice { | UndefinableGPUSupportedLimits } { | provided } { | UndefinableGPUSupportedLimits } => ConvertOption GPURequestDevice "requiredLimits" { | provided } (Undefinable { | UndefinableGPUSupportedLimits }) where
  convertOption _ _ provided = defined all
    where
    all :: { | UndefinableGPUSupportedLimits }
    all = convertOptionsWithDefaults GPURequestDevice defaultGPUSupportedLimits provided

instance ConvertOptionsWithDefaults GPURequestDevice { | GPUQueueDescriptorOptional } { | provided } { | GPUQueueDescriptor } => ConvertOption GPURequestDevice "defaultQueue" { | provided } (Undefinable { | GPUQueueDescriptor }) where
  convertOption _ _ provided = defined all
    where
    all :: { | GPUQueueDescriptor }
    all = convertOptionsWithDefaults GPURequestDevice defaultGPUQueueDescriptor provided

requestDevice
  :: forall provided
   . ConvertOptionsWithDefaults GPURequestDevice { | GPURequestDeviceOptionsOptional } { | provided } { | GPURequestDeviceOptions }
  => GPUAdapter
  -> { | provided }
  -> Effect (Promise (Maybe GPUDevice))
requestDevice g provided = requestDeviceImpl Just Nothing g all
  where
  all :: { | GPURequestDeviceOptions }
  all = convertOptionsWithDefaults GPURequestDevice defaultGPURequestDeviceOptions provided

-- requestAdapterInfo

foreign import requestAdapterInfoImpl :: GPUAdapter -> Array (UnmaskHint) -> Effect (Promise GPUAdapterInfo)

type GPUAdapterInfo =
  { vendor :: String
  , architecture :: String
  , device :: String
  , description :: String
  }

requestAdapterInfo :: GPUAdapter -> Array UnmaskHint -> Effect (Promise GPUAdapterInfo)
requestAdapterInfo = requestAdapterInfoImpl
