module Web.GPU.GPUTextureDimension
  ( GPUTextureDimension
  , oneD
  , threeD
  , twoD
  ) where

import Prelude

newtype GPUTextureDimension = GPUTextureDimension String

derive instance Eq GPUTextureDimension
derive instance Ord GPUTextureDimension
derive newtype instance Show GPUTextureDimension

oneD :: GPUTextureDimension
oneD = GPUTextureDimension "1d"

twoD :: GPUTextureDimension
twoD = GPUTextureDimension "2d"

threeD :: GPUTextureDimension
threeD = GPUTextureDimension "3d"