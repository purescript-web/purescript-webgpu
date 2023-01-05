module Web.GPU.GPURequestAdapterOptions where

import Data.Newtype (class Newtype)
import Web.GPU.GPUPowerPreference (GPUPowerPreference)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPURequestAdapterOptions =
  GPURequestAdapterOptions
    ( RequiredAndOptional ()
        (powerPreference :: GPUPowerPreference, forceFallbackAdapter :: Boolean)
    )

derive instance Newtype GPURequestAdapterOptions _