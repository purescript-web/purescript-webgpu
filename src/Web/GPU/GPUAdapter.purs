-- @inline export features arity=2
-- @inline export limits arity=1
-- @inline export isFallbackAdapter arity=1
-- @inline export requestDevice arity=1
-- @inline export requestAdapterInfo arity=2
module Web.GPU.GPUAdapter
  ( GPUAdapter(..)
  , GPUAdapterInfo
  , features
  , isFallbackAdapter
  , limits
  , requestAdapterInfo
  , requestDevice
  ) where

import Data.Maybe (Maybe(..))
import Effect.Uncurried (EffectFn1, runEffectFn1, EffectFn2, runEffectFn2, EffectFn3, runEffectFn3, EffectFn4, runEffectFn4)
import Data.Set as Set
import Effect (Effect)
import Web.GPU.GPUDeviceDescriptor (GPUDeviceDescriptor)
import Web.GPU.GPUFeatureName (GPUFeatureName)
import Web.GPU.GPUDevice (GPUDevice)
import Web.GPU.GPUSupportedLimits (GPUSupportedLimits)
import Web.GPU.UnmaskHint (UnmaskHint)
import Promise (Promise)

data GPUAdapter

-- features
foreign import featuresImpl
  :: EffectFn3
       (GPUFeatureName -> Set.Set GPUFeatureName -> Set.Set GPUFeatureName)
       (Set.Set GPUFeatureName)
       GPUAdapter
       (Set.Set GPUFeatureName)

features :: GPUAdapter -> Effect (Set.Set GPUFeatureName)
features a = runEffectFn3 featuresImpl Set.insert Set.empty a

foreign import limitsImpl :: EffectFn1 GPUAdapter { | GPUSupportedLimits }

limits :: GPUAdapter -> Effect { | GPUSupportedLimits }
limits a = runEffectFn1 limitsImpl a

foreign import isFallbackAdapterImpl :: EffectFn1 GPUAdapter Boolean

isFallbackAdapter :: GPUAdapter -> Effect Boolean
isFallbackAdapter a = runEffectFn1 isFallbackAdapterImpl a

-- requestDevice

foreign import requestDeviceImpl
  :: EffectFn4 (GPUDevice -> Maybe GPUDevice) (Maybe GPUDevice) GPUAdapter
       GPUDeviceDescriptor
       (Promise (Maybe GPUDevice))

requestDevice
  :: GPUAdapter
  -> GPUDeviceDescriptor
  -> Effect (Promise (Maybe GPUDevice))
requestDevice a b = runEffectFn4 requestDeviceImpl Just Nothing a b

-- requestAdapterInfo

foreign import requestAdapterInfoImpl
  :: EffectFn2 GPUAdapter (Array UnmaskHint) (Promise GPUAdapterInfo)

type GPUAdapterInfo =
  { vendor :: String
  , architecture :: String
  , device :: String
  , description :: String
  }

requestAdapterInfo
  :: GPUAdapter -> Array UnmaskHint -> Effect (Promise GPUAdapterInfo)
requestAdapterInfo a b = runEffectFn2 requestAdapterInfoImpl a b
