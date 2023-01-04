module Web.GPU.GPUPipelineLayoutDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUBindGroupLayout)

newtype GPUPipelineLayoutDescriptor = GPUPipelineLayoutDescriptor
  ( RequiredAndOptional (bindGroupLayouts :: Array GPUBindGroupLayout)
      (label :: String)
  )

derive instance Newtype GPUPipelineLayoutDescriptor _