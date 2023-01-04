module Web.GPU.GPUVertexState where

import Data.Newtype (class Newtype)
import Web.GPU.GPUProgrammableStage (MakeGPUProgrammableStage)
import Web.GPU.GPUVertexBufferLayout (GPUVertexBufferLayout)

newtype GPUVertexState = GPUVertexState
  (MakeGPUProgrammableStage () (buffers :: Array GPUVertexBufferLayout))

derive instance Newtype GPUVertexState _