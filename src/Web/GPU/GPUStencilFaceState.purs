module Web.GPU.GPUStencilFaceState where

import Data.Newtype (class Newtype)
import Web.GPU.GPUCompareFunction (GPUCompareFunction)
import Web.GPU.GPUStencilOperation (GPUStencilOperation)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUStencilFaceState = GPUStencilFaceState
  ( RequiredAndOptional ()
      ( compare :: GPUCompareFunction
      , failOp :: GPUStencilOperation
      , depthFailOp :: GPUStencilOperation
      , passOp :: GPUStencilOperation
      )
  )

derive instance Newtype GPUStencilFaceState _