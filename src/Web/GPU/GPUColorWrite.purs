module Web.GPU.GPUColorWrite
  ( GPUColorWrite
  , GPUColorWriteFlags
  , red
  , green
  , blue
  , alpha
  , all
  ) where

import Data.Int.Bits as Bits
import Web.GPU.Internal.Bitwise (class Bitwise)
import Web.GPU.Internal.Types (UnsignedLong)

newtype GPUColorWrite = GPUColorWrite UnsignedLong

foreign import red :: GPUColorWrite
foreign import green :: GPUColorWrite
foreign import blue :: GPUColorWrite
foreign import alpha :: GPUColorWrite
foreign import all :: GPUColorWrite

instance Bitwise GPUColorWrite where
  or (GPUColorWrite a) (GPUColorWrite b) = GPUColorWrite (a `Bits.or` b)

type GPUColorWriteFlags = GPUColorWrite