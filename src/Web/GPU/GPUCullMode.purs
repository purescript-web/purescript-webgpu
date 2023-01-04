module Web.GPU.GPUCullMode
  ( GPUCullMode
  , back
  , front
  , none
  ) where

import Prelude

newtype GPUCullMode = GPUCullMode String

derive instance Eq GPUCullMode
derive instance Ord GPUCullMode
derive newtype instance Show GPUCullMode

none = GPUCullMode "none" :: GPUCullMode
front = GPUCullMode "front" :: GPUCullMode
back = GPUCullMode "back" :: GPUCullMode