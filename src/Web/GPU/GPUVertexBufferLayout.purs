module Web.GPU.GPUVertexBufferLayout where

import Data.Newtype (class Newtype)
import Web.GPU.GPUVertexAttribute (GPUVertexAttribute)
import Web.GPU.GPUVertexStepMode (GPUVertexStepMode)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUSize64)

newtype GPUVertexBufferLayout = GPUVertexBufferLayout
  ( RequiredAndOptional
      (arrayStride :: GPUSize64, attributes :: Array GPUVertexAttribute)
      (stepMode :: GPUVertexStepMode)
  )

derive instance Newtype GPUVertexBufferLayout _