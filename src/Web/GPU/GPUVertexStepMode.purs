module Web.GPU.GPUVertexStepMode where

import Prelude

newtype GPUVertexStepMode = GPUVertexStepMode String

derive instance Eq GPUVertexStepMode
derive instance Ord GPUVertexStepMode
derive newtype instance Show GPUVertexStepMode

vertex :: GPUVertexStepMode
vertex = GPUVertexStepMode "vertex"

instance' :: GPUVertexStepMode
instance' = GPUVertexStepMode "instance"