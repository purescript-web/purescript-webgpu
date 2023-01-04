module Web.GPU.GPUStorageTextureBindingLayout where

import Data.Newtype (class Newtype)
import Web.GPU.GPUStorageTextureAccess (GPUStorageTextureAccess)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.GPUTextureViewDimension (GPUTextureViewDimension)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUStorageTextureBindingLayout = GPUStorageTextureBindingLayout
  ( RequiredAndOptional (format :: GPUTextureFormat)
      ( viewDimension :: GPUTextureViewDimension
      , access :: GPUStorageTextureAccess
      )
  )

derive instance Newtype GPUStorageTextureBindingLayout _