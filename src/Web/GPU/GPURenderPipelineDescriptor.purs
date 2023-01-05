module Web.GPU.GPURenderPipelineDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUDepthStencilState (GPUDepthStencilState)
import Web.GPU.GPUFragmentState (GPUFragmentState)
import Web.GPU.GPUMultisampleState (GPUMultisampleState)
import Web.GPU.GPUPipelineLayout (GPUPipelineLayout)
import Web.GPU.GPUPrimitiveState (GPUPrimitiveState)
import Web.GPU.GPUVertexState (GPUVertexState)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPURenderPipelineDescriptor = GPURenderPipelineDescriptor
  ( RequiredAndOptional (vertex :: GPUVertexState, layout :: GPUPipelineLayout)
      ( primitive :: GPUPrimitiveState
      , depthStencil :: GPUDepthStencilState
      , multisample :: GPUMultisampleState
      , fragment :: GPUFragmentState
      , label :: String
      )
  )

derive instance Newtype GPURenderPipelineDescriptor _