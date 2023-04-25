-- @inline export expired arity=1
module Web.GPU.GPUExternalTexture
  ( GPUExternalTexture
  , expired
  ) where

import Effect (Effect)

data GPUExternalTexture

foreign import expiredImpl :: GPUExternalTexture -> Effect Boolean

expired :: GPUExternalTexture -> Effect Boolean
expired = expiredImpl