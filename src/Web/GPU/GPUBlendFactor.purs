module Web.GPU.GPUBlendFactor
  ( GPUBlendFactor
  , constant
  , dst
  , dstAlpha
  , one
  , oneMinusConstant
  , oneMinusDst
  , oneMinusDstAlpha
  , oneMinusSrc
  , oneMinusSrcAlpha
  , src
  , srcAlpha
  , srcAlphaSaturated
  , zero
  ) where

import Prelude

newtype GPUBlendFactor = GPUBlendFactor String

derive instance Eq GPUBlendFactor
derive instance Ord GPUBlendFactor
derive newtype instance Show GPUBlendFactor
zero = GPUBlendFactor "zero" :: GPUBlendFactor
one = GPUBlendFactor "one" :: GPUBlendFactor
src = GPUBlendFactor "src" :: GPUBlendFactor
oneMinusSrc = GPUBlendFactor "one-minus-src" :: GPUBlendFactor
srcAlpha = GPUBlendFactor "src-alpha" :: GPUBlendFactor
oneMinusSrcAlpha = GPUBlendFactor "one-minus-src-alpha" :: GPUBlendFactor
dst = GPUBlendFactor "dst" :: GPUBlendFactor
oneMinusDst = GPUBlendFactor "one-minus-dst" :: GPUBlendFactor
dstAlpha = GPUBlendFactor "dst-alpha" :: GPUBlendFactor
oneMinusDstAlpha = GPUBlendFactor "one-minus-dst-alpha" :: GPUBlendFactor
srcAlphaSaturated = GPUBlendFactor "src-alpha-saturated" :: GPUBlendFactor
constant = GPUBlendFactor "constant" :: GPUBlendFactor
oneMinusConstant = GPUBlendFactor "one-minus-constant" :: GPUBlendFactor