-- @inline export gpuOrigin3DX arity=1
-- @inline export gpuOrigin3DXY arity=2
-- @inline export gpuOrigin3DXYZ arity=3
module Web.GPU.GPUOrigin3D
  ( gpuOrigin3DX
  , gpuOrigin3DXY
  , gpuOrigin3DXYZ
  , gpuOrigin3DDict
  , GPUOrigin3D
  ) where

import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.Types (GPUIntegerCoordinate)

data GPUOrigin3D

gpuOrigin3DX :: GPUIntegerCoordinate -> GPUOrigin3D
gpuOrigin3DX x = unsafeCoerce [ x ]

gpuOrigin3DXY :: GPUIntegerCoordinate -> GPUIntegerCoordinate -> GPUOrigin3D
gpuOrigin3DXY x y = unsafeCoerce [ x, y ]

gpuOrigin3DXYZ
  :: GPUIntegerCoordinate
  -> GPUIntegerCoordinate
  -> GPUIntegerCoordinate
  -> GPUOrigin3D
gpuOrigin3DXYZ x y z = unsafeCoerce [ x, y, z ]

gpuOrigin3DDict
  :: { x :: GPUIntegerCoordinate
     , y :: GPUIntegerCoordinate
     , z :: GPUIntegerCoordinate
     }
  -> GPUOrigin3D
gpuOrigin3DDict = unsafeCoerce