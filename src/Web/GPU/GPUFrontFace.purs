module Web.GPU.GPUFrontFace
  ( GPUFrontFace
  , cw
  , ccw
  ) where

import Prelude

newtype GPUFrontFace = GPUFrontFace String

derive instance Eq GPUFrontFace
derive instance Ord GPUFrontFace
derive newtype instance Show GPUFrontFace

ccw :: GPUFrontFace
ccw = GPUFrontFace "ccw"

cw :: GPUFrontFace
cw = GPUFrontFace "cw"