module Web.GPU.GPUShaderModuleCompilationHint where

import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.Types (GPUPipelineLayout, GPUShaderModuleCompilationHint)


auto :: GPUShaderModuleCompilationHint
auto = unsafeCoerce "auto"

hintFromPipelineLayout :: forall layout. GPUPipelineLayout layout -> GPUShaderModuleCompilationHint
hintFromPipelineLayout = unsafeCoerce