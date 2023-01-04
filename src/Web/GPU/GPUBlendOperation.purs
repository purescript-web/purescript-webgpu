module Web.GPU.GPUBlendOperation where

import Prelude

newtype GPUBlendOperation = GPUBlendOperation String

derive instance Eq GPUBlendOperation
derive instance Ord GPUBlendOperation
derive newtype instance Show GPUBlendOperation

add :: GPUBlendOperation
add = GPUBlendOperation "add"

subtract :: GPUBlendOperation
subtract = GPUBlendOperation "subtract"

reverseSubtract :: GPUBlendOperation
reverseSubtract = GPUBlendOperation "reverse-subtract"

min :: GPUBlendOperation
min = GPUBlendOperation "min"

max :: GPUBlendOperation
max = GPUBlendOperation "max"