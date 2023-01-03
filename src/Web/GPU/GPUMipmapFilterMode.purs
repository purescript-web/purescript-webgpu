module Web.GPU.GPUMipmapFilterMode
  ( GPUMipmapFilterMode
  , linear
  , nearest
  ) where

import Prelude

newtype GPUMipmapFilterMode = GPUMipmapFilterMode String

derive instance Eq GPUMipmapFilterMode
derive instance Ord GPUMipmapFilterMode
derive newtype instance Show GPUMipmapFilterMode

nearest :: GPUMipmapFilterMode
nearest = GPUMipmapFilterMode "nearest"

linear :: GPUMipmapFilterMode
linear = GPUMipmapFilterMode "linear"