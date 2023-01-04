module Web.GPU.Internal.RequiredAndOptional
  ( (~)
  , RequiredAndOptional
  , e
  , o
  , r
  , requiredAndOptional
  ) where

import Data.Newtype (class Newtype)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

data RequiredAndOptional (required :: Row Type) (optional :: Row Type)

foreign import requiredAndOptionalImpl :: forall a b c. a -> b -> c

requiredAndOptional
  :: forall nt required optionalL optionalR optional
   . Union optionalL optionalR optional
  => Newtype nt (RequiredAndOptional required optional)
  => { | required }
  -> { | optionalL }
  -> nt
requiredAndOptional = requiredAndOptionalImpl

infixl 4 requiredAndOptional as ~

r
  :: forall nt required optional
   . Newtype nt (RequiredAndOptional required optional)
  => { | required }
  -> nt
r = unsafeCoerce

o
  :: forall nt optionalL optionalR optional
   . Union optionalL optionalR optional
  => Newtype nt (RequiredAndOptional () optional)
  => { | optionalL }
  -> nt
o = unsafeCoerce

e
  :: forall nt optional
   . Newtype nt (RequiredAndOptional () optional)
  => nt
e = unsafeCoerce {}