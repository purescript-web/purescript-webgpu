module Web.GPU.GPUShaderStage
  ( GPUShaderStage
  , vertex
  , fragment
  , compute
  , or
  , (.|.)
  ) where

import Web.GPU.Internal.Unsigned (UnsignedLong)
import Data.Int.Bits as Bits
newtype GPUShaderStage = GPUShaderStage UnsignedLong

foreign import vertex :: GPUShaderStage
foreign import fragment :: GPUShaderStage
foreign import compute :: GPUShaderStage

or :: GPUShaderStage -> GPUShaderStage -> GPUShaderStage
or (GPUShaderStage a) (GPUShaderStage b) = GPUShaderStage (a `Bits.or` b)

infixl 10 or as .|.
