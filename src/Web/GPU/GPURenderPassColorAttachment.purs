module Web.GPU.GPURenderPassColorAttachment where

import Data.Newtype (class Newtype)
import Web.GPU.GPULoadOp (GPULoadOp)
import Web.GPU.GPUStoreOp (GPUStoreOp)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUColor, GPUTextureView)

newtype GPURenderPassColorAttachment = GPURenderPassColorAttachment
  ( RequiredAndOptional
      ( view :: GPUTextureView
      , loadOp :: GPULoadOp
      , storeOp :: GPUStoreOp
      )
      ( resolveTarget :: GPUTextureView
      , clearValue :: GPUColor
      )
  )

derive instance Newtype GPURenderPassColorAttachment _