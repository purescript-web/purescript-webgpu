module Web.GPU.GPUTextureSampleType
  ( GPUTextureSampleType
  , depth
  , float
  , sint
  , uint
  , unfilterableFloat
  ) where

import Prelude

newtype GPUTextureSampleType = GPUTextureSampleType String

derive instance Eq GPUTextureSampleType
derive instance Ord GPUTextureSampleType
derive newtype instance Show GPUTextureSampleType

float :: GPUTextureSampleType
float = GPUTextureSampleType "float"

unfilterableFloat :: GPUTextureSampleType
unfilterableFloat = GPUTextureSampleType "unfilterable-float"

depth :: GPUTextureSampleType
depth = GPUTextureSampleType "depth"

sint :: GPUTextureSampleType
sint = GPUTextureSampleType "sint"

uint :: GPUTextureSampleType
uint = GPUTextureSampleType "uint"