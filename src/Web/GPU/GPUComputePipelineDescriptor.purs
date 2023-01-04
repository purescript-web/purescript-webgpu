module Web.GPU.GPUComputePipelineDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUProgrammableStage (GPUProgrammableStage)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUPipelineLayout)

newtype GPUComputePipelineDescriptor = GPUComputePipelineDescriptor
  ( RequiredAndOptional
      (compute :: GPUProgrammableStage, layout :: GPUPipelineLayout)
      (label :: String)
  )

derive instance Newtype GPUComputePipelineDescriptor _