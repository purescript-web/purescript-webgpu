module Web.GPU.GPUBufferUsage
  ( GPUBufferUsage
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
  , or
  , (.|.)
  ) where

import Web.GPU.Internal.Unsigned (UnsignedLong)
import Data.Int.Bits as Bits
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

or :: GPUBufferUsage -> GPUBufferUsage -> GPUBufferUsage
or (GPUBufferUsage a) (GPUBufferUsage b) = GPUBufferUsage (a `Bits.or` b)

infixl 10 or as .|.
