-- @inline export gpuImpl arity=4
-- @inline export gpu arity=4
module Web.GPU.Navigator (gpu) where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Web.GPU.Internal.Types (GPU)
import Web.HTML.Navigator (Navigator)

foreign import gpuImpl
  :: (GPU -> Maybe GPU) -> Maybe GPU -> Navigator -> Effect (Maybe GPU)

gpu :: Navigator -> Effect (Maybe GPU)
gpu = gpuImpl Just Nothing