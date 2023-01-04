module Web.GPU.GPUExternalTexture where



import Effect (Effect)
import Web.GPU.Internal.Types (GPUExternalTexture)

foreign import expiredImpl :: GPUExternalTexture -> Effect Boolean

expired :: GPUExternalTexture -> Effect Boolean
expired = expiredImpl