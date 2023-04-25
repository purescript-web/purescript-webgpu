module Web.GPU.HTMLCanvasElement
  ( getContext
  ) where

import Data.Maybe (Maybe(..))
import Effect.Uncurried(EffectFn3, runEffectFn3)
import Effect (Effect)
import Web.GPU.GPUCanvasContext (GPUCanvasContext)
import Web.HTML (HTMLCanvasElement)

foreign import getContextImpl :: EffectFn3 (GPUCanvasContext -> Maybe GPUCanvasContext) (Maybe GPUCanvasContext) HTMLCanvasElement  (Maybe GPUCanvasContext)
getContext ∷ HTMLCanvasElement → Effect (Maybe GPUCanvasContext)
getContext a = runEffectFn3 getContextImpl Just Nothing a