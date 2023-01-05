-- @inline export featuresImpl arity=2
-- @inline export features arity=2
-- @inline export limitsImpl arity=1
-- @inline export limits arity=1
-- @inline export isFallbackAdapterImpl arity=1
-- @inline export isFallbackAdapter arity=1
-- @inline export requestDeviceImpl arity=1
-- @inline export requestDevice arity=1
-- @inline export requestAdapterInfoImpl arity=2
-- @inline export requestAdapterInfo arity=2
module Web.GPU.GPUAdapter
  ( GPUAdapterInfo
  , features
  , limits
  , isFallbackAdapter
  , requestAdapterInfo
  , requestDevice
  ) where

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Web.GPU.GPUDeviceDescriptor (GPUDeviceDescriptor)
import Web.GPU.GPUFeatureName (GPUFeatureName)
import Web.GPU.GPUSupportedLimits (GPUSupportedLimits)
import Web.GPU.Internal.Types (GPUAdapter, GPUDevice)
import Web.GPU.UnmaskHint (UnmaskHint)
import Web.Promise (Promise)

-- features
foreign import featuresImpl
  :: (GPUFeatureName -> Set.Set GPUFeatureName -> Set.Set GPUFeatureName)
  -> Set.Set GPUFeatureName
  -> GPUAdapter
  -> Effect (Set.Set GPUFeatureName)

features :: GPUAdapter -> Effect (Set.Set GPUFeatureName)
features = featuresImpl Set.insert Set.empty

foreign import limitsImpl :: GPUAdapter -> Effect { | GPUSupportedLimits }

limits :: GPUAdapter -> Effect { | GPUSupportedLimits }
limits = limitsImpl

foreign import isFallbackAdapterImpl :: GPUAdapter -> Effect Boolean

isFallbackAdapter :: GPUAdapter -> Effect Boolean
isFallbackAdapter = isFallbackAdapterImpl

-- requestDevice

foreign import requestDeviceImpl
  :: (GPUDevice -> Maybe GPUDevice)
  -> Maybe GPUDevice
  -> GPUAdapter
  -> GPUDeviceDescriptor
  -> Effect (Promise (Maybe GPUDevice))

requestDevice
  :: GPUAdapter
  -> GPUDeviceDescriptor
  -> Effect (Promise (Maybe GPUDevice))
requestDevice = requestDeviceImpl Just Nothing

-- requestAdapterInfo

foreign import requestAdapterInfoImpl
  :: GPUAdapter -> Array (UnmaskHint) -> Effect (Promise GPUAdapterInfo)

type GPUAdapterInfo =
  { vendor :: String
  , architecture :: String
  , device :: String
  , description :: String
  }

requestAdapterInfo
  :: GPUAdapter -> Array UnmaskHint -> Effect (Promise GPUAdapterInfo)
requestAdapterInfo = requestAdapterInfoImpl