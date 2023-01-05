module Web.GPU.GPUImageDataLayout where

import Data.Newtype (class Newtype)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUSize64, GPUSize32)

newtype GPUImageDataLayout = GPUImageDataLayout
  ( RequiredAndOptional ()
      ( offset :: GPUSize64
      , bytesPerRow :: GPUSize32
      , rowsPerImage :: GPUSize32
      )
  )

derive instance Newtype GPUImageDataLayout _