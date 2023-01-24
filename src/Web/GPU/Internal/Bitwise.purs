module Web.GPU.Internal.Bitwise (class Bitwise, or, (.|.)) where

import Data.Int.Bits as IntBits
import Data.UInt (UInt)
import Data.UInt as UIntBits

class Bitwise a where
  or :: a -> a -> a

instance Bitwise Int where
  or = IntBits.or

instance Bitwise UInt where
  or = UIntBits.or

infixl 10 or as .|.