module Web.GPU.GPUTextureAspect
  ( GPUTextureAspect
  , all
  , depthOnly
  , stencilOnly
  ) where

import Prelude

newtype GPUTextureAspect = GPUTextureAspect String

derive instance Eq GPUTextureAspect
derive instance Ord GPUTextureAspect
derive newtype instance Show GPUTextureAspect

all :: GPUTextureAspect
all = GPUTextureAspect "all"

stencilOnly :: GPUTextureAspect
stencilOnly = GPUTextureAspect "stencil-only"

depthOnly :: GPUTextureAspect
depthOnly = GPUTextureAspect "depth-only"