module Web.GPU.Internal.Types
  ( GPUBufferDynamicOffset
  , GPUDepthBias
  , GPUIndex32
  , GPUIntegerCoordinate
  , GPUSampleMask
  , GPUSignedOffset32
  , GPUSize32
  , GPUSize64
  , GPUStencilValue
  , ImageBitmap
  , Long
  , OffscreenCanvas
  , UnsignedLong
  , UnsignedLongLong
  , UnsignedShort
  ) where

-- todo: add this to the PS ecosystem
data ImageBitmap
-- todo: add this to the PS ecosystem
data OffscreenCanvas
type Long = Int
type UnsignedShort = Int
type UnsignedLong = Int
type UnsignedLongLong = Int
type GPUSize64 = UnsignedLongLong
type GPUIntegerCoordinate = UnsignedLong
type GPUSize32 = UnsignedLong
type GPUIndex32 = UnsignedLong
type GPUStencilValue = UnsignedLong
type GPUSampleMask = UnsignedLong
type GPUDepthBias = Long
type GPUBufferDynamicOffset = UnsignedLong
type GPUSignedOffset32 = Long