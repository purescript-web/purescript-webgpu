module Web.GPU.GPUPipelineLayoutDescriptor
  ( (:++)
  , add
  , empty
  ) where

import Prim.Int (class ToString)

import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList)
import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.RowLength (class RowLength)
import Web.GPU.Internal.Types (GPUBindGroupLayoutDescriptor, GPUPipelineLayoutDescriptor)

empty :: GPUPipelineLayoutDescriptor ()
empty = unsafeCoerce { bindGroupLayouts: [] }

foreign import unsafeAdd :: forall x y z. x -> y -> z

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