module Web.GPU.GPUStoreOp
  ( GPUStoreOp
  , discard
  , store
  ) where

import Prelude

newtype GPUStoreOp = GPUStoreOp String

derive instance Eq GPUStoreOp
derive instance Ord GPUStoreOp
derive newtype instance Show GPUStoreOp

store :: GPUStoreOp
store = GPUStoreOp "store"

discard :: GPUStoreOp
discard = GPUStoreOp "discard"