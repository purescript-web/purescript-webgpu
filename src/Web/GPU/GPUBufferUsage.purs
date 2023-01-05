module Web.GPU.GPUBufferUsage
  ( GPUBufferUsage
  , GPUBufferUsageFlags
  , copyDst
  , copySrc
  , index
  , indirect
  , mapRead
  , mapWrite
  , queryResolve
  , storage
  , uniform
  , vertex
  ) where

import Data.Int.Bits as Bits
import Web.GPU.Internal.Bitwise (class Bitwise)
import Web.GPU.Internal.Types (UnsignedLong)

newtype GPUBufferUsage = GPUBufferUsage UnsignedLong

foreign import mapRead :: GPUBufferUsage
foreign import mapWrite :: GPUBufferUsage
foreign import copySrc :: GPUBufferUsage
foreign import copyDst :: GPUBufferUsage
foreign import index :: GPUBufferUsage
foreign import vertex :: GPUBufferUsage
foreign import uniform :: GPUBufferUsage
foreign import storage :: GPUBufferUsage
foreign import indirect :: GPUBufferUsage
foreign import queryResolve :: GPUBufferUsage

instance Bitwise GPUBufferUsage where
  or (GPUBufferUsage a) (GPUBufferUsage b) = GPUBufferUsage (a `Bits.or` b)

type GPUBufferUsageFlags = GPUBufferUsage