module Web.GPU.GPUAddressMode
  ( GPUAddressMode
  , clampToEdge
  , mirrorRepeat
  , repeat
  ) where

import Prelude

newtype GPUAddressMode = GPUAddressMode String

derive instance Eq GPUAddressMode
derive instance Ord GPUAddressMode
derive newtype instance Show GPUAddressMode

clampToEdge :: GPUAddressMode
clampToEdge = GPUAddressMode "clamp-to-edge"

repeat :: GPUAddressMode
repeat = GPUAddressMode "repeat"

mirrorRepeat :: GPUAddressMode
mirrorRepeat = GPUAddressMode "mirror-repeat"