module Web.GPU.GPUOrigin3D
  ( gpuOrigin3DX
  , gpuOrigin3DXY
  , gpuOrigin3DXYZ
  , gpuOrigin3DDict
  ) where

import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.Types (GPUOrigin3D)
import Web.GPU.Internal.Unsigned (GPUIntegerCoordinate)

gpuOrigin3DX :: GPUIntegerCoordinate -> GPUOrigin3D
gpuOrigin3DX x = unsafeCoerce [ x ]

gpuOrigin3DXY :: GPUIntegerCoordinate -> GPUIntegerCoordinate -> GPUOrigin3D
gpuOrigin3DXY x y = unsafeCoerce [ x, y ]

gpuOrigin3DXYZ :: GPUIntegerCoordinate -> GPUIntegerCoordinate -> GPUIntegerCoordinate -> GPUOrigin3D
gpuOrigin3DXYZ x y z = unsafeCoerce [ x, y, z ]

gpuOrigin3DDict :: { x :: GPUIntegerCoordinate, y :: GPUIntegerCoordinate, z :: GPUIntegerCoordinate } -> GPUOrigin3D
gpuOrigin3DDict = unsafeCoerce