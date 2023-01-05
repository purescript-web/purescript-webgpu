module Web.GPU.GPURenderPassTimestampWrite where

import Data.Newtype (class Newtype)
import Web.GPU.GPUQuerySet (GPUQuerySet)
import Web.GPU.GPURenderPassTimestampLocation (GPURenderPassTimestampLocation)
import Web.GPU.Internal.RequiredAndOptional (RequiredAndOptional)
import Web.GPU.Internal.Types (GPUSize32)

newtype GPURenderPassTimestampWrite = GPURenderPassTimestampWrite
  ( RequiredAndOptional
      ( querySet :: GPUQuerySet
      , queryIndex :: GPUSize32
      , location :: GPURenderPassTimestampLocation
      )
      ()
  )

derive instance Newtype GPURenderPassTimestampWrite _