module Web.GPU.GPUBindGroupDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUBindGroupEntry (GPUBindGroupEntry)
import Web.GPU.GPUBindGroupLayout (GPUBindGroupLayout)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUBindGroupDescriptor = GPUBindGroupDescriptor
  ( RequiredAndOptional
      (layout :: GPUBindGroupLayout, entries :: Array GPUBindGroupEntry)
      (label :: String)
  )

derive instance Newtype GPUBindGroupDescriptor _