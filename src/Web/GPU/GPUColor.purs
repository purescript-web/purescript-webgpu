-- @inline export gpuColorRGBA arity=4
module Web.GPU.GPUColor
  ( GPUColor
  , gpuColorDict
  , gpuColorRGBA
  ) where

import Unsafe.Coerce (unsafeCoerce)

data GPUColor

gpuColorRGBA :: Number -> Number -> Number -> Number -> GPUColor
gpuColorRGBA r g b a = unsafeCoerce [ r, g, b, a ]

gpuColorDict
  :: { r :: Number, g :: Number, b :: Number, a :: Number } -> GPUColor
gpuColorDict rgba = unsafeCoerce rgba