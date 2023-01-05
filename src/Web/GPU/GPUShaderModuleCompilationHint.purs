-- @inline export hintFromPipelineLayout arity=1
module Web.GPU.GPUShaderModuleCompilationHint where

import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.Types (GPUPipelineLayout, GPUShaderModuleCompilationHint)

auto :: GPUShaderModuleCompilationHint
auto = unsafeCoerce "auto"

hintFromPipelineLayout :: GPUPipelineLayout -> GPUShaderModuleCompilationHint
hintFromPipelineLayout = unsafeCoerce