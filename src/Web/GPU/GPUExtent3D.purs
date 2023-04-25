-- @inline export gpuExtent3DW arity=1
-- @inline export gpuExtent3DWH arity=2
-- @inline export gpuExtent3DWHD arity=3
module Web.GPU.GPUExtent3D where

import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.Types (GPUIntegerCoordinate)

data GPUExtent3D

gpuExtent3DW :: GPUIntegerCoordinate -> GPUExtent3D
gpuExtent3DW w = unsafeCoerce [ w ]

gpuExtent3DWH :: GPUIntegerCoordinate -> GPUIntegerCoordinate -> GPUExtent3D
gpuExtent3DWH w h = unsafeCoerce [ w, h ]

gpuExtent3DWHD
  :: GPUIntegerCoordinate
  -> GPUIntegerCoordinate
  -> GPUIntegerCoordinate
  -> GPUExtent3D
gpuExtent3DWHD w h d = unsafeCoerce [ w, h, d ]

gpuExtent3DDict
  :: { width :: GPUIntegerCoordinate
     , height :: GPUIntegerCoordinate
     , depthOrArrayLayers :: GPUIntegerCoordinate
     }
  -> GPUExtent3D
gpuExtent3DDict = unsafeCoerce