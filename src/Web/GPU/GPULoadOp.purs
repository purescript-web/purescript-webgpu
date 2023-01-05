module Web.GPU.GPULoadOp
  ( GPULoadOp
  , clear
  , load
  ) where

import Prelude

newtype GPULoadOp = GPULoadOp String

derive instance Eq GPULoadOp
derive instance Ord GPULoadOp
derive newtype instance Show GPULoadOp

load :: GPULoadOp
load = GPULoadOp "load"

clear :: GPULoadOp
clear = GPULoadOp "clear"