module Web.GPU.GPUCanvasConfiguration where

import Data.Newtype (class Newtype)
import Web.GPU.GPUCanvasAlphaMode (GPUCanvasAlphaMode)
import Web.GPU.GPUTextureFormat (GPUTextureFormat)
import Web.GPU.GPUTextureUsage (GPUTextureUsageFlags)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.PredefinedColorSpace (PredefinedColorSpace)
import Web.GPU.GPUDevice (GPUDevice)

newtype GPUCanvasConfiguration = GPUCanvasConfiguration
  ( RequiredAndOptional
      ( device :: GPUDevice
      , format :: GPUTextureFormat

      )
      ( usage :: GPUTextureUsageFlags
      , viewFormats :: Array GPUTextureFormat
      , colorSpace :: PredefinedColorSpace
      , alphaMode :: GPUCanvasAlphaMode

      )
  )

derive instance Newtype GPUCanvasConfiguration _