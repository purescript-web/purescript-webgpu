module Web.GPU.GPUBindGroupDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUBindGroupEntry, GPUBindGroupLayout)

newtype GPUBindGroupDescriptor = GPUBindGroupDescriptor
  ( RequiredAndOptional
      (layout :: GPUBindGroupLayout, entries :: Array GPUBindGroupEntry)
      (label :: String)
  )

derive instance Newtype GPUBindGroupDescriptor _