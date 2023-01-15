-- @inline export canvas arity=1
-- @inline export configure arity=2
-- @inline export unconfigure arity=1
-- @inline export getCurrentTexture arity=1
module Web.GPU.GPUCanvasContext
  ( GPUCanvasContext
  , canvas
  , configure
  , getCurrentTexture
  , unconfigure
  ) where

import Prelude

import Effect (Effect)
import Web.GPU.GPUCanvasConfiguration (GPUCanvasConfiguration)
import Web.GPU.GPUTexture (GPUTexture)
import Web.HTML (HTMLCanvasElement)

data GPUCanvasContext

foreign import canvasImpl :: GPUCanvasContext -> Effect HTMLCanvasElement

canvas :: GPUCanvasContext -> Effect HTMLCanvasElement
canvas = canvasImpl

foreign import configureImpl
  :: GPUCanvasContext -> GPUCanvasConfiguration -> Effect Unit

configure :: GPUCanvasContext -> GPUCanvasConfiguration -> Effect Unit
configure = configureImpl

foreign import unconfigureImpl :: GPUCanvasContext -> Effect Unit

unconfigure :: GPUCanvasContext -> Effect Unit
unconfigure = unconfigureImpl

foreign import getCurrentTextureImpl :: GPUCanvasContext -> Effect GPUTexture

getCurrentTexture :: GPUCanvasContext -> Effect GPUTexture
getCurrentTexture = getCurrentTextureImpl