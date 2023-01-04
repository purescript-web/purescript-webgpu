module Web.GPU.GPUTextureBindingLayout where

import Data.Newtype (class Newtype)
import Web.GPU.GPUTextureSampleType (GPUTextureSampleType)
import Web.GPU.GPUTextureViewDimension (GPUTextureViewDimension)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUTextureBindingLayout = GPUTextureBindingLayout
  ( RequiredAndOptional ()
      ( sampleType :: GPUTextureSampleType
      , viewDimension :: GPUTextureViewDimension
      , multisampled :: Boolean
      )
  )

derive instance Newtype GPUTextureBindingLayout _