module Web.GPU.GPUImageCopyBuffer where

import Data.Newtype (class Newtype)
import Web.GPU.GPUBuffer (GPUBuffer)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUSize64, GPUSize32)

newtype GPUImageCopyBuffer = GPUImageCopyBuffer
  ( RequiredAndOptional
      ( buffer :: GPUBuffer
      )
      ( offset :: GPUSize64
      , bytesPerRow :: GPUSize32
      , rowsPerImage :: GPUSize32
      )
  )

derive instance Newtype GPUImageCopyBuffer _