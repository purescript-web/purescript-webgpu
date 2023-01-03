module Web.GPU.GPUBindGroupLayoutDescriptor
  ( (:)
  , GPUBufferBindingLayout
  , GPUExternalTextureBindingLayout
  , GPUSamplerBindingLayout
  , GPUStorageTextureBindingLayout
  , GPUTextureBindingLayout
  , add
  , empty
  ) where

import Prim.Int (class Compare, class ToString)
import Prim.Ordering (GT, LT, Ordering)
import Prim.Row (class Cons, class Lacks)
import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.Types (Binding, GPUBindGroupLayoutDescriptor, GPUBindGroupLayoutEntry)

empty :: GPUBindGroupLayoutDescriptor ()
empty = unsafeCoerce { entries: [] }

foreign import data GPUBufferBindingLayout :: Binding
foreign import data GPUSamplerBindingLayout :: Binding
foreign import data GPUTextureBindingLayout :: Binding
foreign import data GPUStorageTextureBindingLayout :: Binding
foreign import data GPUExternalTextureBindingLayout :: Binding
foreign import unsafeAdd :: forall x y z. x -> y -> z

class Neq (neq :: Ordering)

instance Neq LT
instance Neq GT

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

infixr 5 add as :