module Web.GPU.PredefinedColorSpace
  ( PredefinedColorSpace
  , displayP3
  , srgb
  ) where

import Prelude

newtype PredefinedColorSpace = PredefinedColorSpace String

derive instance Eq PredefinedColorSpace
derive instance Ord PredefinedColorSpace
derive newtype instance Show PredefinedColorSpace

srgb :: PredefinedColorSpace
srgb = PredefinedColorSpace "srgb"

displayP3 :: PredefinedColorSpace
displayP3 = PredefinedColorSpace "display-p3"