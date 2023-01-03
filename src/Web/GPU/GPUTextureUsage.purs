module Web.GPU.GPUTextureUsage
  ( GPUTextureUsage
  , copyDst
  , copySrc
  , or
  , (.|.)
  ) where

import Web.GPU.Internal.Unsigned (UnsignedLong)
import Data.Int.Bits as Bits

newtype GPUTextureUsage = GPUTextureUsage UnsignedLong

foreign import copySrc :: GPUTextureUsage
foreign import copyDst :: GPUTextureUsage
foreign import textureBinding :: GPUTextureUsage
foreign import storageBinding :: GPUTextureUsage
foreign import renderAttachment :: GPUTextureUsage

or :: GPUTextureUsage -> GPUTextureUsage -> GPUTextureUsage
or (GPUTextureUsage a) (GPUTextureUsage b) = GPUTextureUsage (a `Bits.or` b)

infixl 10 or as .|.
