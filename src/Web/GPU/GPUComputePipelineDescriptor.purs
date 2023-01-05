module Web.GPU.GPUComputePipelineDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUPipelineLayout (GPUPipelineLayout)
import Web.GPU.GPUProgrammableStage (GPUProgrammableStage)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUComputePipelineDescriptor = GPUComputePipelineDescriptor
  ( RequiredAndOptional
      (compute :: GPUProgrammableStage, layout :: GPUPipelineLayout)
      (label :: String)
  )

derive instance Newtype GPUComputePipelineDescriptor _