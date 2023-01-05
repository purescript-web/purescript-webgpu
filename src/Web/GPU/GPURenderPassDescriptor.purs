module Web.GPU.GPURenderPassDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUQuerySet (GPUQuerySet)
import Web.GPU.GPURenderPassColorAttachment (GPURenderPassColorAttachment)
import Web.GPU.GPURenderPassDepthStencilAttachment (GPURenderPassDepthStencilAttachment)
import Web.GPU.GPURenderPassTimestampWrite (GPURenderPassTimestampWrite)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUSize64)

newtype GPURenderPassDescriptor = GPURenderPassDescriptor
  ( RequiredAndOptional
      ( colorAttachments :: Array GPURenderPassColorAttachment
      )
      ( depthStencilAttachment :: GPURenderPassDepthStencilAttachment
      , occlusionQuerySet :: GPUQuerySet
      , timestampWrites :: Array GPURenderPassTimestampWrite
      , maxDrawCount :: GPUSize64
      , label :: String
      )
  )

derive instance Newtype GPURenderPassDescriptor _