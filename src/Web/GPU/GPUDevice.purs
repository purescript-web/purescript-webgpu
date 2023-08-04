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
  ) where

import Prelude

import Data.Set as Set
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
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
import Promise (Promise)

data GPUDevice

-- features
foreign import featuresImpl
  :: EffectFn3
       (GPUFeatureName -> Set.Set GPUFeatureName -> Set.Set GPUFeatureName)
       (Set.Set GPUFeatureName)
       GPUDevice
       (Set.Set GPUFeatureName)

features :: GPUDevice -> Effect (Set.Set GPUFeatureName)
features a = runEffectFn3 featuresImpl Set.insert Set.empty a

-- limits
foreign import limitsImpl :: EffectFn1 GPUDevice { | GPUSupportedLimits }

limits :: GPUDevice -> Effect { | GPUSupportedLimits }
limits a = runEffectFn1 limitsImpl a

-- queue

foreign import queueImpl :: EffectFn1 GPUDevice GPUQueue

queue :: GPUDevice -> Effect GPUQueue
queue a = runEffectFn1 queueImpl a

-- destroy

foreign import destroyImpl :: EffectFn1 GPUDevice Unit

destroy :: GPUDevice -> Effect Unit
destroy a = runEffectFn1 destroyImpl a

-- createBuffer
foreign import createBufferImpl
  :: EffectFn2 GPUDevice GPUBufferDescriptor GPUBuffer

createBuffer
  :: GPUDevice
  -> GPUBufferDescriptor
  -> Effect GPUBuffer
createBuffer a b = runEffectFn2 createBufferImpl a b

-- createTexture
foreign import createTextureImpl
  :: EffectFn2 GPUDevice GPUTextureDescriptor GPUTexture

createTexture
  :: GPUDevice
  -> GPUTextureDescriptor
  -> Effect GPUTexture
createTexture a b = runEffectFn2 createTextureImpl a b

-- createSampler

foreign import createSamplerImpl
  :: EffectFn2 GPUDevice GPUSamplerDescriptor GPUSampler

createSampler
  :: GPUDevice
  -> GPUSamplerDescriptor
  -> Effect GPUSampler
createSampler a b = runEffectFn2 createSamplerImpl a b

-- importExternalTexture

foreign import importExternalTextureImpl
  :: EffectFn2 GPUDevice GPUExternalTextureDescriptor GPUExternalTexture

importExternalTexture
  :: GPUDevice
  -> GPUExternalTextureDescriptor
  -> Effect GPUExternalTexture
importExternalTexture a b = runEffectFn2 importExternalTextureImpl a b

-- createBindGroupLayout

foreign import createBindGroupLayoutImpl
  :: EffectFn2 GPUDevice GPUBindGroupLayoutDescriptor GPUBindGroupLayout

createBindGroupLayout
  :: GPUDevice
  -> GPUBindGroupLayoutDescriptor
  -> Effect GPUBindGroupLayout
createBindGroupLayout a b = runEffectFn2 createBindGroupLayoutImpl a b

-- createPipelineLayout

foreign import createPipelineLayoutImpl
  :: EffectFn2 GPUDevice GPUPipelineLayoutDescriptor GPUPipelineLayout

createPipelineLayout
  :: GPUDevice
  -> GPUPipelineLayoutDescriptor
  -> Effect GPUPipelineLayout
createPipelineLayout a b = runEffectFn2 createPipelineLayoutImpl a b

-- createBindGroup

foreign import createBindGroupImpl
  :: EffectFn2 GPUDevice GPUBindGroupDescriptor GPUBindGroup

createBindGroup :: GPUDevice -> GPUBindGroupDescriptor -> Effect GPUBindGroup
createBindGroup a b = runEffectFn2 createBindGroupImpl a b

-- createShaderModule

foreign import createShaderModuleImpl
  :: EffectFn2 GPUDevice GPUShaderModuleDescriptor GPUShaderModule

createShaderModule
  :: GPUDevice
  -> GPUShaderModuleDescriptor
  -> Effect GPUShaderModule
createShaderModule a b = runEffectFn2 createShaderModuleImpl a b

-- createComputePipeline
foreign import createComputePipelineImpl
  :: EffectFn2 GPUDevice GPUComputePipelineDescriptor GPUComputePipeline

createComputePipeline
  :: GPUDevice
  -> GPUComputePipelineDescriptor
  -> Effect GPUComputePipeline
createComputePipeline a b = runEffectFn2 createComputePipelineImpl a b

-- createRenderPipeline

foreign import createRenderPipelineImpl
  :: EffectFn2 GPUDevice GPURenderPipelineDescriptor GPURenderPipeline

createRenderPipeline
  :: GPUDevice
  -> GPURenderPipelineDescriptor
  -> Effect GPURenderPipeline
createRenderPipeline a b = runEffectFn2 createRenderPipelineImpl a b

-- createComputePipelineAsnyc
foreign import createComputePipelineAsyncImpl
  :: EffectFn2 GPUDevice GPUComputePipelineDescriptor
       (Promise GPUComputePipeline)

createComputePipelineAsnyc
  :: GPUDevice
  -> GPUComputePipelineDescriptor
  -> Effect (Promise GPUComputePipeline)
createComputePipelineAsnyc a b = runEffectFn2 createComputePipelineAsyncImpl a b

-- createRenderPipelineAsync

foreign import createRenderPipelineAsyncImpl
  :: EffectFn2 GPUDevice GPURenderPipelineDescriptor (Promise GPURenderPipeline)

createRenderPipelineAsync
  :: GPUDevice
  -> GPURenderPipelineDescriptor
  -> Effect (Promise GPURenderPipeline)
createRenderPipelineAsync a b = runEffectFn2 createRenderPipelineAsyncImpl a b

-- createCommandEncoder

foreign import createCommandEncoderImpl
  :: EffectFn2 GPUDevice GPUCommandEncoderDescriptor GPUCommandEncoder

createCommandEncoder
  :: GPUDevice -> GPUCommandEncoderDescriptor -> Effect GPUCommandEncoder
createCommandEncoder a b = runEffectFn2 createCommandEncoderImpl a b

-- createRenderBundleEncoder

foreign import createRenderBundleEncoderImpl
  :: EffectFn2 GPUDevice GPURenderBundleEncoderDescriptor GPUBuffer

createRenderBundleEncoder
  :: GPUDevice
  -> GPURenderBundleEncoderDescriptor
  -> Effect GPUBuffer
createRenderBundleEncoder a b = runEffectFn2 createRenderBundleEncoderImpl a b

-- createQuerySet

foreign import createQuerySetImpl
  :: EffectFn2 GPUDevice GPUQuerySetDescriptor GPUQuerySet

createQuerySet :: GPUDevice -> GPUQuerySetDescriptor -> Effect GPUQuerySet
createQuerySet a b = runEffectFn2 createQuerySetImpl a b

toEventTarget :: GPUDevice -> EventTarget
toEventTarget = unsafeCoerce
