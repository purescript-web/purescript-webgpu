module Web.GPU.GPUExtent3D where

import Data.Array (singleton)
import Data.Tuple.Nested ((/\), type (/\))
import Web.GPU.Internal.ConvertibleOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Web.GPU.Internal.Undefinable (Undefinable, defined, undefined)
import Web.GPU.Internal.Unsigned (GPUIntegerCoordinate)

newtype GPUExtent3DDict = GPUExtent3DDict
  { width :: GPUIntegerCoordinate
  , height :: Undefinable GPUIntegerCoordinate
  , depthOrArrayLayers :: Undefinable GPUIntegerCoordinate
  }

class IsGPUExtent3D (b :: Type)

instance IsGPUExtent3D (Array GPUIntegerCoordinate)
instance IsGPUExtent3D GPUExtent3DDict

class IsGPUExtent3D b <= AsGPUExtent3D a b | a -> b where
  asGPUExtent3D :: a -> b

instance AsGPUExtent3D GPUIntegerCoordinate (Array GPUIntegerCoordinate) where
  asGPUExtent3D = singleton

instance AsGPUExtent3D (GPUIntegerCoordinate /\ GPUIntegerCoordinate) (Array GPUIntegerCoordinate) where
  asGPUExtent3D (a /\ b) = [ a, b ]

instance AsGPUExtent3D (GPUIntegerCoordinate /\ GPUIntegerCoordinate /\ GPUIntegerCoordinate) (Array GPUIntegerCoordinate) where
  asGPUExtent3D (a /\ b /\ c) = [ a, b, c ]

type GPUExtent3DDictOptional =
  ( height :: Undefinable GPUIntegerCoordinate
  , depthOrArrayLayers :: Undefinable GPUIntegerCoordinate
  )

type GPUExtent3DDictAll =
  ( width :: GPUIntegerCoordinate
  | GPUExtent3DDictOptional
  )

defaultGPUExtent3DOptions :: { | GPUExtent3DDictOptional }
defaultGPUExtent3DOptions =
  { height: undefined
  , depthOrArrayLayers: undefined
  }

data Extent3DDict = Extent3DDict

instance ConvertOption Extent3DDict "height" GPUIntegerCoordinate (Undefinable GPUIntegerCoordinate) where
  convertOption _ _ = defined

instance ConvertOption Extent3DDict "depthOrArrayLayers" GPUIntegerCoordinate (Undefinable GPUIntegerCoordinate) where
  convertOption _ _ = defined

instance ConvertOptionsWithDefaults Extent3DDict { | GPUExtent3DDictOptional } { | provided } { | GPUExtent3DDictAll } => AsGPUExtent3D { | provided } GPUExtent3DDict where
  asGPUExtent3D provided = GPUExtent3DDict (convertOptionsWithDefaults Extent3DDict defaultGPUExtent3DOptions provided)
