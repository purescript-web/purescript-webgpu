module Web.GPU.GPUCompareFunction where

import Prelude

newtype GPUCompareFunction = GPUCompareFunction String

derive instance Eq GPUCompareFunction
derive instance Ord GPUCompareFunction
derive newtype instance Show GPUCompareFunction
never = GPUCompareFunction "never" :: GPUCompareFunction
less = GPUCompareFunction "less" :: GPUCompareFunction
equal = GPUCompareFunction "equal" :: GPUCompareFunction
lessEqual = GPUCompareFunction "less-equal" :: GPUCompareFunction
greater = GPUCompareFunction "greater" :: GPUCompareFunction
notEqual = GPUCompareFunction "not-equal" :: GPUCompareFunction
greaterEqual = GPUCompareFunction "greater-equal" :: GPUCompareFunction
always = GPUCompareFunction "always" :: GPUCompareFunction