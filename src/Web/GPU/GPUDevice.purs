module Web.GPU.Device
  ( createBindGroup
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
  ) where

import Prelude

import Data.Set as Set
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Internal.Types (EventTarget)
import Web.GPU.GPUBindGroupDescriptor (GPUBindGroupDescriptor)
import Web.GPU.GPUBindGroupLayoutDescriptor (GPUBindGroupLayoutDescriptor)
import Web.GPU.GPUBufferDescriptor (GPUBufferDescriptor)
import Web.GPU.GPUCommandEncoderDescriptor (GPUCommandEncoderDescriptor)
import Web.GPU.GPUComputePipelineDescriptor (GPUComputePipelineDescriptor)
import Web.GPU.GPUExternalTextureDescriptor (GPUExternalTextureDescriptor)
import Web.GPU.GPUFeatureName (GPUFeatureName)
import Web.GPU.GPUPipelineLayoutDescriptor (GPUPipelineLayoutDescriptor)
import Web.GPU.GPUQuerySetDescriptor (GPUQuerySetDescriptor)
import Web.GPU.GPURenderBundleEncoderDescriptor (GPURenderBundleEncoderDescriptor)
import Web.GPU.GPURenderPipelineDescriptor (GPURenderPipelineDescriptor)
import Web.GPU.GPUSamplerDescriptor (GPUSamplerDescriptor)
import Web.GPU.GPUShaderModuleDescriptor (GPUShaderModuleDescriptor)
import Web.GPU.GPUSupportedLimits (GPUSupportedLimits)
import Web.GPU.GPUTextureDescriptor (GPUTextureDescriptor)
import Web.GPU.Internal.Types (GPUBindGroup, GPUBindGroupLayout, GPUBuffer, GPUCommandEncoder, GPUComputePipeline, GPUDevice, GPUExternalTexture, GPUPipelineLayout, GPUQuerySet, GPUQueue, GPURenderPipeline, GPUSampler, GPUShaderModule, GPUTexture)
import Web.Promise (Promise)

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