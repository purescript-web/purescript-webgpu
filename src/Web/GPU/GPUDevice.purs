-- @inline export features arity=2
-- @inline export limits arity=1
-- @inline export queue arity=1
-- @inline export destroy arity=1
-- @inline export createBuffer arity=2
-- @inline export createTexture arity=2
-- @inline export createSampler arity=2
-- @inline export importExternalTexture arity=2
-- @inline export createBindGroupLayout arity=2
-- @inline export createPipelineLayout arity=2
-- @inline export createBindGroup arity=2
-- @inline export createShaderModule arity=2
-- @inline export createComputePipeline arity=2
-- @inline export createRenderPipeline arity=2
-- @inline export createComputePipelineAsnyc arity=2
-- @inline export createRenderPipelineAsync arity=2
-- @inline export createCommandEncoder arity=2
-- @inline export createRenderBundleEncoder arity=2
-- @inline export createQuerySet arity=2
-- @inline export toEventTarget arity=1
module Web.GPU.GPUDevice
  ( GPUDevice
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
  , toEventTarget
  )
  where

import Prelude

import Data.Set as Set
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Internal.Types (EventTarget)
import Web.GPU.GPUBindGroup (GPUBindGroup)
import Web.GPU.GPUBindGroupDescriptor (GPUBindGroupDescriptor)
import Web.GPU.GPUBindGroupLayout (GPUBindGroupLayout)
import Web.GPU.GPUBindGroupLayoutDescriptor (GPUBindGroupLayoutDescriptor)
import Web.GPU.GPUBuffer (GPUBuffer)
import Web.GPU.GPUBufferDescriptor (GPUBufferDescriptor)
import Web.GPU.GPUCommandEncoder (GPUCommandEncoder)
import Web.GPU.GPUCommandEncoderDescriptor (GPUCommandEncoderDescriptor)
import Web.GPU.GPUComputePipeline (GPUComputePipeline)
import Web.GPU.GPUComputePipelineDescriptor (GPUComputePipelineDescriptor)
import Web.GPU.GPUExternalTexture (GPUExternalTexture)
import Web.GPU.GPUExternalTextureDescriptor (GPUExternalTextureDescriptor)
import Web.GPU.GPUFeatureName (GPUFeatureName)
import Web.GPU.GPUPipelineLayout (GPUPipelineLayout)
import Web.GPU.GPUPipelineLayoutDescriptor (GPUPipelineLayoutDescriptor)
import Web.GPU.GPUQuerySet (GPUQuerySet)
import Web.GPU.GPUQuerySetDescriptor (GPUQuerySetDescriptor)
import Web.GPU.GPUQueue (GPUQueue)
import Web.GPU.GPURenderBundleEncoderDescriptor (GPURenderBundleEncoderDescriptor)
import Web.GPU.GPURenderPipeline (GPURenderPipeline)
import Web.GPU.GPURenderPipelineDescriptor (GPURenderPipelineDescriptor)
import Web.GPU.GPUSampler (GPUSampler)
import Web.GPU.GPUSamplerDescriptor (GPUSamplerDescriptor)
import Web.GPU.GPUShaderModule (GPUShaderModule)
import Web.GPU.GPUShaderModuleDescriptor (GPUShaderModuleDescriptor)
import Web.GPU.GPUSupportedLimits (GPUSupportedLimits)
import Web.GPU.GPUTexture (GPUTexture)
import Web.GPU.GPUTextureDescriptor (GPUTextureDescriptor)
import Web.Promise (Promise)

data GPUDevice

-- features
foreign import featuresImpl
  :: (GPUFeatureName -> Set.Set GPUFeatureName -> Set.Set GPUFeatureName)
  -> Set.Set GPUFeatureName
  -> GPUDevice
  -> Effect (Set.Set GPUFeatureName)

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
foreign import createBufferImpl
  :: GPUDevice -> GPUBufferDescriptor -> Effect GPUBuffer

createBuffer
  :: GPUDevice
  -> GPUBufferDescriptor
  -> Effect GPUBuffer
createBuffer = createBufferImpl

-- createTexture
foreign import createTextureImpl
  :: GPUDevice -> GPUTextureDescriptor -> Effect GPUTexture

createTexture
  :: GPUDevice
  -> GPUTextureDescriptor
  -> Effect GPUTexture
createTexture = createTextureImpl

-- createSampler

foreign import createSamplerImpl
  :: GPUDevice -> GPUSamplerDescriptor -> Effect GPUSampler

createSampler
  :: GPUDevice
  -> GPUSamplerDescriptor
  -> Effect GPUSampler
createSampler = createSamplerImpl

-- importExternalTexture

foreign import importExternalTextureImpl
  :: GPUDevice
  -> GPUExternalTextureDescriptor
  -> Effect GPUExternalTexture

importExternalTexture
  :: GPUDevice
  -> GPUExternalTextureDescriptor
  -> Effect GPUExternalTexture
importExternalTexture = importExternalTextureImpl

-- createBindGroupLayout

foreign import createBindGroupLayoutImpl
  :: GPUDevice
  -> GPUBindGroupLayoutDescriptor
  -> Effect GPUBindGroupLayout

createBindGroupLayout
  :: GPUDevice
  -> GPUBindGroupLayoutDescriptor
  -> Effect GPUBindGroupLayout
createBindGroupLayout = createBindGroupLayoutImpl

-- createPipelineLayout

foreign import createPipelineLayoutImpl
  :: GPUDevice -> GPUPipelineLayoutDescriptor -> Effect GPUPipelineLayout

createPipelineLayout
  :: GPUDevice
  -> GPUPipelineLayoutDescriptor
  -> Effect GPUPipelineLayout
createPipelineLayout = createPipelineLayoutImpl

-- createBindGroup

foreign import createBindGroupImpl
  :: GPUDevice -> GPUBindGroupDescriptor -> Effect GPUBindGroup

createBindGroup :: GPUDevice -> GPUBindGroupDescriptor -> Effect GPUBindGroup
createBindGroup = createBindGroupImpl

-- createShaderModule

foreign import createShaderModuleImpl
  :: GPUDevice -> GPUShaderModuleDescriptor -> Effect GPUShaderModule

createShaderModule
  :: GPUDevice
  -> GPUShaderModuleDescriptor
  -> Effect GPUShaderModule
createShaderModule = createShaderModuleImpl

-- createComputePipeline
foreign import createComputePipelineImpl
  :: GPUDevice
  -> GPUComputePipelineDescriptor
  -> Effect GPUComputePipeline

createComputePipeline
  :: GPUDevice
  -> GPUComputePipelineDescriptor
  -> Effect GPUComputePipeline
createComputePipeline = createComputePipelineImpl

-- createRenderPipeline

foreign import createRenderPipelineImpl
  :: GPUDevice
  -> GPURenderPipelineDescriptor
  -> Effect GPURenderPipeline

createRenderPipeline
  :: GPUDevice
  -> GPURenderPipelineDescriptor
  -> Effect GPURenderPipeline
createRenderPipeline = createRenderPipelineImpl

-- createComputePipelineAsnyc
foreign import createComputePipelineAsyncImpl
  :: GPUDevice
  -> GPUComputePipelineDescriptor
  -> Effect (Promise GPUComputePipeline)

createComputePipelineAsnyc
  :: GPUDevice
  -> GPUComputePipelineDescriptor
  -> Effect (Promise GPUComputePipeline)
createComputePipelineAsnyc = createComputePipelineAsyncImpl

-- createRenderPipelineAsync

foreign import createRenderPipelineAsyncImpl
  :: GPUDevice
  -> GPURenderPipelineDescriptor
  -> Effect (Promise GPURenderPipeline)

createRenderPipelineAsync
  :: GPUDevice
  -> GPURenderPipelineDescriptor
  -> Effect (Promise GPURenderPipeline)
createRenderPipelineAsync = createRenderPipelineAsyncImpl

-- createCommandEncoder

foreign import createCommandEncoderImpl
  :: GPUDevice -> GPUCommandEncoderDescriptor -> Effect GPUCommandEncoder

createCommandEncoder
  :: GPUDevice -> GPUCommandEncoderDescriptor -> Effect GPUCommandEncoder
createCommandEncoder = createCommandEncoderImpl

-- createRenderBundleEncoder

foreign import createRenderBundleEncoderImpl
  :: GPUDevice -> GPURenderBundleEncoderDescriptor -> Effect GPUBuffer

createRenderBundleEncoder
  :: GPUDevice
  -> GPURenderBundleEncoderDescriptor
  -> Effect GPUBuffer
createRenderBundleEncoder = createRenderBundleEncoderImpl

-- createQuerySet

foreign import createQuerySetImpl
  :: GPUDevice -> GPUQuerySetDescriptor -> Effect GPUQuerySet

createQuerySet :: GPUDevice -> GPUQuerySetDescriptor -> Effect GPUQuerySet
createQuerySet = createQuerySetImpl

toEventTarget :: GPUDevice -> EventTarget
toEventTarget = unsafeCoerce