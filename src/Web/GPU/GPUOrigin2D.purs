-- @inline export gpuOrigin2DX arity=1
-- @inline export gpuOrigin2DXY arity=2
module Web.GPU.GPUOrigin2D
  ( GPUOrigin2D
  , gpuOrigin2DX
  , gpuOrigin2DXY
  , gpuOrigin2DDict
  ) where

import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.Types (GPUIntegerCoordinate)

data GPUOrigin2D

gpuOrigin2DX :: GPUIntegerCoordinate -> GPUOrigin2D
gpuOrigin2DX x = unsafeCoerce [ x ]

gpuOrigin2DXY :: GPUIntegerCoordinate -> GPUIntegerCoordinate -> GPUOrigin2D
gpuOrigin2DXY x y = unsafeCoerce [ x, y ]

gpuOrigin2DDict
  :: { x :: GPUIntegerCoordinate
     , y :: GPUIntegerCoordinate
     }
  -> GPUOrigin2D
gpuOrigin2DDict = unsafeCoerce