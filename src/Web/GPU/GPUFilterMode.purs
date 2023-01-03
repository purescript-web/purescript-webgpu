module Web.GPU.GPUFilterMode
  ( GPUFilterMode
  , linear
  , nearest
  ) where

import Prelude

newtype GPUFilterMode = GPUFilterMode String

derive instance Eq GPUFilterMode
derive instance Ord GPUFilterMode
derive newtype instance Show GPUFilterMode

nearest :: GPUFilterMode
nearest = GPUFilterMode "nearest"

linear :: GPUFilterMode
linear = GPUFilterMode "linear"