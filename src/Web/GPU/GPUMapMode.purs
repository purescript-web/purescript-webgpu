module Web.GPU.GPUMapMode
  ( GPUMapMode
  , read
  , write
  ) where

import Data.Int.Bits as Bits
import Web.GPU.Internal.Bitwise (class Bitwise)
import Web.GPU.Internal.Types (UnsignedLong)

newtype GPUMapMode = GPUMapMode UnsignedLong

foreign import read :: GPUMapMode
foreign import write :: GPUMapMode

instance Bitwise GPUMapMode where
  or (GPUMapMode a) (GPUMapMode b) = GPUMapMode (a `Bits.or` b)