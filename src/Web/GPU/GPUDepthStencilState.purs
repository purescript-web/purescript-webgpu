module Web.GPU.GPUDepthStencilState where

import Data.Newtype (class Newtype)
import Web.GPU.GPUCompareFunction (GPUCompareFunction)
import Web.GPU.GPUStencilFaceState (GPUStencilFaceState)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUStencilValue, GPUDepthBias)

newtype GPUDepthStencilState = GPUDepthStencilState
  ( RequiredAndOptional
      (format :: GPUTextureFormat)
      ( depthWriteEnabled :: Boolean
      , depthCompare :: GPUCompareFunction
      , stencilFront :: GPUStencilFaceState
      , stencilBack :: GPUStencilFaceState
      , stencilReadMask :: GPUStencilValue
      , stencilWriteMask :: GPUStencilValue
      , depthBias :: GPUDepthBias
      , depthBiasSlopeScale :: Number
      , depthBiasClamp :: Number
      )
  )

derive instance Newtype GPUDepthStencilState _