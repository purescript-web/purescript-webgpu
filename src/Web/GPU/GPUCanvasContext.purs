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
import Effect.Uncurried(EffectFn1, runEffectFn1,EffectFn2, runEffectFn2)

import Effect (Effect)
import Web.GPU.GPUCanvasConfiguration (GPUCanvasConfiguration)
import Web.GPU.GPUTexture (GPUTexture)
import Web.HTML (HTMLCanvasElement)

data GPUCanvasContext

foreign import canvasImpl :: EffectFn1 GPUCanvasContext  HTMLCanvasElement
canvas :: GPUCanvasContext -> Effect HTMLCanvasElement
canvas a = runEffectFn1 canvasImpl a

foreign import configureImpl :: EffectFn2 GPUCanvasContext GPUCanvasConfiguration  Unit
configure :: GPUCanvasContext -> GPUCanvasConfiguration -> Effect Unit
configure a b = runEffectFn2 configureImpl a b

foreign import unconfigureImpl :: EffectFn1 GPUCanvasContext  Unit
unconfigure :: GPUCanvasContext -> Effect Unit
unconfigure a = runEffectFn1 unconfigureImpl a

foreign import getCurrentTextureImpl :: EffectFn1 GPUCanvasContext  GPUTexture
getCurrentTexture :: GPUCanvasContext -> Effect GPUTexture
getCurrentTexture a = runEffectFn1 getCurrentTextureImpl a