module Web.GPU.GPUStencilOperation
  ( GPUStencilOperation
  , decrementClamp
  , decrementWrap
  , incrementClamp
  , incrementWrap
  , invert
  , keep
  , replace
  , zero
  ) where

import Prelude

newtype GPUStencilOperation = GPUStencilOperation String

derive instance Eq GPUStencilOperation
derive instance Ord GPUStencilOperation
derive newtype instance Show GPUStencilOperation

keep :: GPUStencilOperation
keep = GPUStencilOperation "keep"

zero :: GPUStencilOperation
zero = GPUStencilOperation "zero"

replace :: GPUStencilOperation
replace = GPUStencilOperation "replace"

invert :: GPUStencilOperation
invert = GPUStencilOperation "invert"

incrementClamp :: GPUStencilOperation
incrementClamp = GPUStencilOperation "increment-clamp"

decrementClamp :: GPUStencilOperation
decrementClamp = GPUStencilOperation "decrement-clamp"

incrementWrap :: GPUStencilOperation
incrementWrap = GPUStencilOperation "increment-wrap"

decrementWrap :: GPUStencilOperation
decrementWrap = GPUStencilOperation "decrement-wrap"