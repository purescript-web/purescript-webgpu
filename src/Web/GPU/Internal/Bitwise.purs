module Web.GPU.Internal.Bitwise (class Bitwise, or, (.|.)) where

class Bitwise a where
  or :: a -> a -> a

infixl 10 or as .|.