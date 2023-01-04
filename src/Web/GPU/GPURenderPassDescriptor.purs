module Web.GPU.GPURenderPassDescriptor where



import Web.GPU.GPURenderPassColorAttachment (GPURenderPassColorAttachment)
import Web.GPU.GPURenderPassDepthStencilAttachment (GPURenderPassDepthStencilAttachment)
import Web.GPU.GPURenderPassTimestampWrite (GPURenderPassTimestampWrite)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUQuerySet, GPUSize64)

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