module Web.GPU.GPUShaderStage
  ( GPUShaderStage
  , GPUShaderStageFlags
  , vertex
  , fragment
  , compute
  ) where

import Data.Int.Bits as Bits
import Web.GPU.Internal.Bitwise (class Bitwise)
import Web.GPU.Internal.Types (UnsignedLong)

newtype GPUShaderStage = GPUShaderStage UnsignedLong

foreign import vertex :: GPUShaderStage
foreign import fragment :: GPUShaderStage
foreign import compute :: GPUShaderStage

instance Bitwise GPUShaderStage where
  or (GPUShaderStage a) (GPUShaderStage b) = GPUShaderStage (a `Bits.or` b)

type GPUShaderStageFlags = GPUShaderStage