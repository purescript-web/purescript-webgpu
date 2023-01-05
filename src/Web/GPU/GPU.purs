-- @inline export requestAdapterImpl arity=1
-- @inline export requestAdapter arity=1
-- @inline export getPreferredCanvasFormatImpl arity=1
-- @inline export getPreferredCanvasFormat arity=1
module Web.GPU.GPU
  ( requestAdapter
  , getPreferredCanvasFormat
  ) where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.GPU.GPURequestAdapterOptions (GPURequestAdapterOptions)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.Internal.Types (GPU, GPUAdapter)
import Web.Promise (Promise)

-- requestAdapter

foreign import requestAdapterImpl
  :: (GPUAdapter -> Maybe GPUAdapter)
  -> Maybe GPUAdapter
  -> GPU
  -> GPURequestAdapterOptions
  -> Effect (Promise (Maybe GPUAdapter))

requestAdapter
  :: GPU
  -> GPURequestAdapterOptions
  -> Effect (Promise (Maybe GPUAdapter))
requestAdapter = requestAdapterImpl Just Nothing

-- getPreferredCanvasFormat

foreign import getPreferredCanvasFormatImpl :: GPU -> Effect GPUTextureFormat

getPreferredCanvasFormat :: GPU -> Effect GPUTextureFormat
getPreferredCanvasFormat = getPreferredCanvasFormatImpl