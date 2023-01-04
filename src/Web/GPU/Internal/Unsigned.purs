module Web.GPU.Internal.Unsigned
  ( GPUDepthBias
  , GPUIndex32
  , GPUIntegerCoordinate
  , GPUSampleMask
  , GPUSize32
  , GPUSize64
  , GPUStencilValue
  , Long
  , UnsignedLong
  , UnsignedLongLong
  , UnsignedShort
  ) where

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