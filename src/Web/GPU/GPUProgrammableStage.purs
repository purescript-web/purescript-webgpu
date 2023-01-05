module Web.GPU.GPUProgrammableStage where

import Data.Newtype (class Newtype)
import Foreign.Object (Object)
import Web.GPU.GPUShaderModule (GPUShaderModule)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

type MakeGPUProgrammableStage r o = RequiredAndOptional
  (module :: GPUShaderModule, entryPoint :: String | r)
  (constants :: Object GPUPipelineConstantValue | o)

newtype GPUProgrammableStage = GPUProgrammableStage
  (MakeGPUProgrammableStage () ())

type GPUPipelineConstantValue = Number

derive instance Newtype GPUProgrammableStage _