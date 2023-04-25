module Web.GPU.GPUPipelineBase where

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Web.GPU.GPUBindGroupLayout (GPUBindGroupLayout)
import Web.GPU.GPUComputePipeline (GPUComputePipeline)
import Web.GPU.GPURenderPipeline (GPURenderPipeline)
import Web.GPU.Internal.Types (UnsignedLong)

foreign import unsafeGetBindGroupLayoutImpl :: forall a. EffectFn2 a UnsignedLong  GPUBindGroupLayout
class GPUPipelineBase a where
  getBindGroupLayout :: a -> UnsignedLong -> Effect GPUBindGroupLayout

instance GPUPipelineBase GPUComputePipeline where
  getBindGroupLayout a b = runEffectFn2 unsafeGetBindGroupLayoutImpl a b

instance GPUPipelineBase GPURenderPipeline where
  getBindGroupLayout a b = runEffectFn2 unsafeGetBindGroupLayoutImpl a b