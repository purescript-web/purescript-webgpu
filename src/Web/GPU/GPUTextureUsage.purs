module Web.GPU.GPUTextureUsage
  ( GPUTextureUsage
  , copyDst
  , copySrc
  ) where

import Data.Int.Bits as Bits
import Web.GPU.Internal.Bitwise (class Bitwise)
import Web.GPU.Internal.Unsigned (UnsignedLong)

newtype GPUTextureUsage = GPUTextureUsage UnsignedLong

foreign import copySrc :: GPUTextureUsage
foreign import copyDst :: GPUTextureUsage
foreign import textureBinding :: GPUTextureUsage
foreign import storageBinding :: GPUTextureUsage
foreign import renderAttachment :: GPUTextureUsage

instance Bitwise GPUTextureUsage where
  or (GPUTextureUsage a) (GPUTextureUsage b) = GPUTextureUsage (a `Bits.or` b)
