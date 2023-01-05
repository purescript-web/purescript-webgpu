module Web.GPU.GPUPipelineLayoutDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUBindGroupLayout (GPUBindGroupLayout)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUPipelineLayoutDescriptor = GPUPipelineLayoutDescriptor
  ( RequiredAndOptional (bindGroupLayouts :: Array GPUBindGroupLayout)
      (label :: String)
  )

derive instance Newtype GPUPipelineLayoutDescriptor _