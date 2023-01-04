module Web.GPU.Internal.Undefinable
  ( defined
  , undefined
  , undefinedImpl
  , Undefinable
  ) where

import Unsafe.Coerce (unsafeCoerce)

data Undefinable :: forall k. k -> Type
data Undefinable a

foreign import undefinedImpl :: forall a. Undefinable a

undefined :: forall a. Undefinable a
undefined = undefinedImpl

defined :: forall a. a -> Undefinable a
defined = unsafeCoerce