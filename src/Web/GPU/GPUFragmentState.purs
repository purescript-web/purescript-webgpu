module Web.GPU.GPUFragmentState where

import Data.Newtype (class Newtype)
import Web.GPU.GPUColorTargetState (GPUColorTargetState)
import Web.GPU.GPUProgrammableStage (MakeGPUProgrammableStage)

newtype GPUFragmentState = GPUFragmentState
  (MakeGPUProgrammableStage (targets :: Array GPUColorTargetState) ())

derive instance Newtype GPUFragmentState _