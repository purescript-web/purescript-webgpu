module Web.GPU.GPURenderPassDepthStencilAttachment where

import Data.Newtype (class Newtype)
import Web.GPU.GPULoadOp (GPULoadOp)
import Web.GPU.GPUStoreOp (GPUStoreOp)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUTextureView, GPUStencilValue)

newtype GPURenderPassDepthStencilAttachment =
  GPURenderPassDepthStencilAttachment
    ( RequiredAndOptional
        ( view :: GPUTextureView
        )
        ( depthClearValue :: Number
        , depthLoadOp :: GPULoadOp
        , depthStoreOp :: GPUStoreOp
        , depthReadOnly :: Boolean
        , stencilClearValue :: GPUStencilValue
        , stencilLoadOp :: GPULoadOp
        , stencilStoreOp :: GPUStoreOp
        , stencilReadOnly :: Boolean
        )
    )

derive instance Newtype GPURenderPassDepthStencilAttachment _