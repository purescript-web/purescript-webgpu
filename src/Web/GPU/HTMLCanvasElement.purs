-- @inline export getContextImpl arity=1
module Web.GPU.HTMLCanvasElement
  ( getContext
  ) where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.GPU.GPUCanvasContext (GPUCanvasContext)
import Web.HTML (HTMLCanvasElement)

foreign import getContextImpl
  :: (GPUCanvasContext -> Maybe GPUCanvasContext)
  -> Maybe GPUCanvasContext
  -> HTMLCanvasElement
  -> Effect (Maybe GPUCanvasContext)

getContext ∷ HTMLCanvasElement → Effect (Maybe GPUCanvasContext)
getContext = getContextImpl Just Nothing