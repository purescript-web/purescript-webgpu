module Web.GPU.GPUPipelineBase where

import Effect (Effect)
import Web.GPU.Internal.Types (GPUBindGroupLayout, GPUComputePipeline, GPURenderPipeline)
import Web.GPU.Internal.Unsigned (UnsignedLong)

foreign import unsafeGetBindGroupLayoutImpl :: forall a. a -> UnsignedLong -> Effect GPUBindGroupLayout

class GPUPipelineBase a where
  getBindGroupLayout :: a -> UnsignedLong -> Effect GPUBindGroupLayout

instance GPUPipelineBase GPUComputePipeline where
  getBindGroupLayout = unsafeGetBindGroupLayoutImpl
instance GPUPipelineBase GPURenderPipeline where
  getBindGroupLayout = unsafeGetBindGroupLayoutImpl