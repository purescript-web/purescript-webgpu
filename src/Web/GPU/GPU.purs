-- @inline export requestAdapter arity=1
-- @inline export getPreferredCanvasFormat arity=1
module Web.GPU.GPU
  ( GPU
  , requestAdapter
  , getPreferredCanvasFormat
  ) where

import Data.Maybe (Maybe(..))
import Effect.Uncurried (EffectFn1, runEffectFn1, EffectFn4, runEffectFn4)
import Effect (Effect)
import Web.GPU.GPURequestAdapterOptions (GPURequestAdapterOptions)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Promise (Promise)
import Web.GPU.GPUAdapter (GPUAdapter)

data GPU

-- requestAdapter

foreign import requestAdapterImpl
  :: EffectFn4 (GPUAdapter -> Maybe GPUAdapter) (Maybe GPUAdapter) GPU
       GPURequestAdapterOptions
       (Promise (Maybe GPUAdapter))

requestAdapter
  :: GPU
  -> GPURequestAdapterOptions
  -> Effect (Promise (Maybe GPUAdapter))
requestAdapter a b = runEffectFn4 requestAdapterImpl Just Nothing a b

-- getPreferredCanvasFormat

foreign import getPreferredCanvasFormatImpl :: EffectFn1 GPU GPUTextureFormat

getPreferredCanvasFormat :: GPU -> Effect GPUTextureFormat
getPreferredCanvasFormat a = runEffectFn1 getPreferredCanvasFormatImpl a
