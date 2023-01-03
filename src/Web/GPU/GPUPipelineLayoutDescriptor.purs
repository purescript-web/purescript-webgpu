module Web.GPU.GPUPipelineLayoutDescriptor
  ( (:++)
  , GPUBufferBindingLayout
  , GPUExternalTextureBindingLayout
  , GPUSamplerBindingLayout
  , GPUStorageTextureBindingLayout
  , GPUTextureBindingLayout
  , add
  , empty
  ) where

import Prim.Int (class ToString)
import Prim.Ordering (GT, LT, Ordering)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList)
import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.RowLength (class RowLength)
import Web.GPU.Internal.Types (Binding, GPUBindGroupLayoutDescriptor, GPUPipelineLayoutDescriptor)

empty :: GPUPipelineLayoutDescriptor ()
empty = unsafeCoerce { bindGroupLayouts: [] }

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
  :: forall i entries entriesRL key prev current
   . RowToList entries entriesRL
  => RowLength entriesRL i
  => ToString i key
  => Lacks key prev
  => Cons key (GPUBindGroupLayoutDescriptor entries) prev current
  => GPUBindGroupLayoutDescriptor entries
  -> GPUPipelineLayoutDescriptor prev
  -> GPUPipelineLayoutDescriptor current
add = unsafeAdd

infixr 5 add as :++