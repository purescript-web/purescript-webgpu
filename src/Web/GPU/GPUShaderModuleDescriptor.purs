module Web.GPU.GPUShaderModuleDescriptor where

import Data.Newtype (class Newtype)
import Foreign (Foreign)
import Foreign.Object (Object)
import Web.GPU.GPUShaderModuleCompilationHint (GPUShaderModuleCompilationHint)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUShaderModuleDescriptor = GPUShaderModuleDescriptor
  ( RequiredAndOptional (code :: String)
      ( sourceMap :: Foreign
      , hints :: Object GPUShaderModuleCompilationHint
      , label :: String
      )
  )

derive instance Newtype GPUShaderModuleDescriptor _