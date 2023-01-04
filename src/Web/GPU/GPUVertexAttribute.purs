module Web.GPU.GPUVertexAttribute where

import Data.Newtype (class Newtype)
import Web.GPU.GPUVertexFormat (GPUVertexFormat)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUSize64, GPUIndex32)

newtype GPUVertexAttribute = GPUVertexAttribute
  ( RequiredAndOptional
      ( format :: GPUVertexFormat
      , offset :: GPUSize64
      , shaderLocation :: GPUIndex32
      )
      ()
  )

derive instance Newtype GPUVertexAttribute _