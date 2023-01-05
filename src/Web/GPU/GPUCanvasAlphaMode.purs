module Web.GPU.GPUCanvasAlphaMode
  ( GPUCanvasAlphaMode
  , opaque
  , premultiplied
  ) where

import Prelude

newtype GPUCanvasAlphaMode = GPUCanvasAlphaMode String

derive instance Eq GPUCanvasAlphaMode
derive instance Ord GPUCanvasAlphaMode
derive newtype instance Show GPUCanvasAlphaMode

opaque :: GPUCanvasAlphaMode
opaque = GPUCanvasAlphaMode "opaque"

premultiplied :: GPUCanvasAlphaMode
premultiplied = GPUCanvasAlphaMode "premultiplied"