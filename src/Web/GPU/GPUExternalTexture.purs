-- @inline export expired arity=1
module Web.GPU.GPUExternalTexture
  ( GPUExternalTexture
  , expired
  ) where

import Effect (Effect)
import Effect.Uncurried(EffectFn1, runEffectFn1)

data GPUExternalTexture

foreign import expiredImpl :: EffectFn1 GPUExternalTexture  Boolean
expired :: GPUExternalTexture -> Effect Boolean
expired a = runEffectFn1 expiredImpl a