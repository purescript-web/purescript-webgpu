module Web.GPU.GPUBindGroupLayoutDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUBindGroupLayoutEntry)

newtype GPUBindGroupLayoutDescriptor = GPUBindGroupLayoutDescriptor
  ( RequiredAndOptional (entries :: Array GPUBindGroupLayoutEntry)
      (label :: String)
  )

derive instance Newtype GPUBindGroupLayoutDescriptor _