module Web.GPU.GPUBufferMapState
  ( GPUBufferMapState
  , mapped
  , pending
  , unmapped
  ) where

newtype GPUBufferMapState = GPUBufferMapState String

unmapped :: GPUBufferMapState
unmapped = GPUBufferMapState "unmapped"

pending :: GPUBufferMapState
pending = GPUBufferMapState "pending"

mapped :: GPUBufferMapState
mapped = GPUBufferMapState "mapped"