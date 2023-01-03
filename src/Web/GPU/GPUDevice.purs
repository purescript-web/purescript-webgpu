module WebGPU
  ( features
  , limits
  , destroy
  , queue
  ) where

import Prelude

import Data.Set as Set
import Effect (Effect)
import Web.GPU.Internal.Types (GPUDevice, GPUQueue)
import Web.GPU.GPUFeatureName (GPUFeatureName)
import Web.GPU.GPUSupportedLimits (GPUSupportedLimits)

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

foreign import destroyImpl :: GPUDevice -> Effect Unit

destroy :: GPUDevice -> Effect Unit
destroy = destroyImpl