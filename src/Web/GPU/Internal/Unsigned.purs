module Web.GPU.Internal.Unsigned
  ( GPUIntegerCoordinate
  , GPUSize32
  , GPUSize64
  , UnsignedLong
  , UnsignedLongLong
  )
  where

type UnsignedLong = Int
type UnsignedLongLong = Int
type GPUSize64 = UnsignedLongLong
type GPUIntegerCoordinate = UnsignedLong
type GPUSize32 = UnsignedLong