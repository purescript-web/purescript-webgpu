module Web.GPU.GPUShaderModuleDescriptor where

import Data.Newtype (class Newtype)
import Foreign (Foreign)
import Foreign.Object (Object)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUShaderModuleCompilationHint)

newtype GPUShaderModuleDescriptor = GPUShaderModuleDescriptor
  ( RequiredAndOptional (code :: String)
      ( sourceMap :: Foreign
      , hints :: Object GPUShaderModuleCompilationHint
      , label :: String
      )
  )

derive instance Newtype GPUShaderModuleDescriptor _