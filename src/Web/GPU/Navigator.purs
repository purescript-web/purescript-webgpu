-- @inline export gpu arity=4
module Web.GPU.Navigator (gpu) where

import Data.Maybe (Maybe(..))
import Effect.Uncurried(EffectFn3, runEffectFn3)
import Effect (Effect)
import Web.GPU.GPU (GPU)
import Web.HTML.Navigator (Navigator)

foreign import gpuImpl :: EffectFn3 (GPU -> Maybe GPU) (Maybe GPU) Navigator  (Maybe GPU)
gpu :: Navigator -> Effect (Maybe GPU)
gpu a = runEffectFn3 gpuImpl Just Nothing a