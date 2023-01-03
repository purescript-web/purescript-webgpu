module WebGPU
  ( createBuffer
  , destroy
  , features
  , limits
  , queue
  , BufferDescriptor
  )
  where

import Prelude

import Data.Set as Set
import Effect (Effect)
import Web.GPU.GPUBufferUsage (GPUBufferUsage)
import Web.GPU.GPUFeatureName (GPUFeatureName)
import Web.GPU.GPUSupportedLimits (GPUSupportedLimits)
import Web.GPU.Internal.ConvertibleOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Web.GPU.Internal.Types (GPUBuffer, GPUDevice, GPUQueue)
import Web.GPU.Internal.Undefinable (Undefinable, defined, undefined)
import Web.GPU.Internal.Unsigned (GPUSize64)

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

type GPUBufferDescriptorOptional =
  ( mappedAtCreation :: Undefinable Boolean
  , label :: Undefinable String
  )

type GPUBufferDescriptor =
  ( size :: GPUSize64 ,
    usage :: GPUBufferUsage
  | GPUBufferDescriptorOptional
  )

defaultGPUBufferDescriptorOptions :: { | GPUBufferDescriptorOptional }
defaultGPUBufferDescriptorOptions =
  { mappedAtCreation: undefined
  , label: undefined
  }

data BufferDescriptor = BufferDescriptor

instance  ConvertOption BufferDescriptor "mappedAtCreation" Boolean (Undefinable Boolean) where
  convertOption _ _ = defined

instance  ConvertOption BufferDescriptor "label" String (Undefinable String) where
  convertOption _ _ = defined

foreign import createBufferImpl :: GPUDevice -> {|GPUBufferDescriptor} -> Effect GPUBuffer

createBuffer
  :: forall provided
   . ConvertOptionsWithDefaults BufferDescriptor { | GPUBufferDescriptorOptional } { | provided } { | GPUBufferDescriptor }
  =>GPUDevice -> { | provided }
  -> Effect GPUBuffer
createBuffer gpuDevice provided = createBufferImpl gpuDevice all
  where
  all :: { | GPUBufferDescriptor }
  all = convertOptionsWithDefaults BufferDescriptor defaultGPUBufferDescriptorOptions provided