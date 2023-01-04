module Web.GPU.GPUBindGroupLayoutDescriptor
  ( (:+)
  , add
  , empty
  ) where

import Prim.Int (class Compare, class ToString)
import Prim.Ordering (LT)
import Prim.Row (class Cons, class Lacks)
import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.Types (GPUBindGroupLayoutDescriptor, GPUBindGroupLayoutEntry)

empty :: GPUBindGroupLayoutDescriptor ()
empty = unsafeCoerce { entries: [] }
foreign import unsafeAdd :: forall x y z. x -> y -> z

add
  :: forall i key binding prev current
   . Compare (-1) i LT
  => ToString i key
  => Lacks key prev
  => Cons key binding prev current
  => GPUBindGroupLayoutEntry i binding
  -> GPUBindGroupLayoutDescriptor prev
  -> GPUBindGroupLayoutDescriptor current
add = unsafeAdd

infixr 5 add as :+