module Web.GPU.GPUPrimitiveState where

import Data.Newtype (class Newtype)
import Web.GPU.GPUCullMode (GPUCullMode)
import Web.GPU.GPUFrontFace (GPUFrontFace)
import Web.GPU.GPUIndexFormat (GPUIndexFormat)
import Web.GPU.GPUPrimitiveTopology (GPUPrimitiveTopology)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUPrimitiveState = GPUPrimitiveState
  ( RequiredAndOptional ()
      ( topology :: GPUPrimitiveTopology
      , stripIndexFormat :: GPUIndexFormat
      , frontFace :: GPUFrontFace
      , cullMode :: GPUCullMode
      , unclippedDepth :: Boolean
      )
  )

derive instance Newtype GPUPrimitiveState _