module Web.GPU.Internal.Unsigned
  ( GPUIntegerCoordinate
  , GPUSize32
  , GPUSize64
  , GPUIndex32
  , UnsignedShort
  , UnsignedLong
  , UnsignedLongLong
  )
  where

type UnsignedShort = Int
type UnsignedLong = Int
type UnsignedLongLong = Int
type GPUSize64 = UnsignedLongLong
type GPUIntegerCoordinate = UnsignedLong
type GPUSize32 = UnsignedLong
type GPUIndex32 = UnsignedLong