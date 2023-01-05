module Web.GPU.GPUIndexFormat
  ( GPUIndexFormat
  , uint16
  , uint32
  ) where

import Prelude

newtype GPUIndexFormat = GPUIndexFormat String

derive instance Eq GPUIndexFormat
derive instance Ord GPUIndexFormat
derive newtype instance Show GPUIndexFormat

uint16 :: GPUIndexFormat
uint16 = GPUIndexFormat "uint16"

uint32 :: GPUIndexFormat
uint32 = GPUIndexFormat "uint32"