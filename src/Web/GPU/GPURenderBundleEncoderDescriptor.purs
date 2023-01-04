module Web.GPU.GPURenderBundleEncoderDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUSize32)

newtype GPURenderBundleEncoderDescriptor = GPURenderBundleEncoderDescriptor
  ( RequiredAndOptional
      ( colorFormats :: Array GPUTextureFormat
      )
      ( depthReadOnly :: Boolean
      , stencilReadOnly :: Boolean
      , depthStencilFormat :: GPUTextureFormat
      , sampleCount :: GPUSize32
      , label :: String
      )
  )

derive instance Newtype GPURenderBundleEncoderDescriptor _