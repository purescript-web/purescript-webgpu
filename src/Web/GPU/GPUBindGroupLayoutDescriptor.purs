module Web.GPU.GPUBindGroupLayoutDescriptor where

import Data.Newtype (class Newtype)
import Web.GPU.GPUBindGroupLayoutEntry (GPUBindGroupLayoutEntry)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)

newtype GPUBindGroupLayoutDescriptor = GPUBindGroupLayoutDescriptor
  ( RequiredAndOptional (entries :: Array GPUBindGroupLayoutEntry)
      (label :: String)
  )

derive instance Newtype GPUBindGroupLayoutDescriptor _