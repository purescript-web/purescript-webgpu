module Web.GPU.GPUImageCopyTextureTagged where

import Data.Newtype (class Newtype)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.PredefinedColorSpace (PredefinedColorSpace)

newtype GPUImageCopyTextureTagged = GPUImageCopyTextureTagged
  ( RequiredAndOptional ()
      ( colorSpace :: PredefinedColorSpace
      , premultipliedAlpha :: Boolean
      )
  )

derive instance Newtype GPUImageCopyTextureTagged _