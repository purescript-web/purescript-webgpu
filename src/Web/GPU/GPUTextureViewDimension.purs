module Web.GPU.GPUTextureViewDimension where

import Prelude

newtype GPUTextureViewDimension = GPUTextureViewDimension String

derive instance Eq GPUTextureViewDimension
derive instance Ord GPUTextureViewDimension
derive newtype instance Show GPUTextureViewDimension

oneD :: GPUTextureViewDimension
oneD = GPUTextureViewDimension "1d"

twoD :: GPUTextureViewDimension
twoD = GPUTextureViewDimension "2d"

twoDArray :: GPUTextureViewDimension
twoDArray = GPUTextureViewDimension "2d-array"

cube :: GPUTextureViewDimension
cube = GPUTextureViewDimension "cube"

cubeArray :: GPUTextureViewDimension
cubeArray = GPUTextureViewDimension "cube-array"

threeD :: GPUTextureViewDimension
threeD = GPUTextureViewDimension "3d"