module Web.GPU.BufferSource where

import Data.ArrayBuffer.Types (ArrayBuffer, DataView, Float32Array, Float64Array, Int16Array, Int32Array, Int8Array, Uint16Array, Uint32Array, Uint8Array, Uint8ClampedArray)
import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.Types (BufferSource)

fromArrayBuffer :: ArrayBuffer -> BufferSource
fromArrayBuffer = unsafeCoerce

fromInt8Array :: Int8Array -> BufferSource
fromInt8Array = unsafeCoerce

fromInt16Array :: Int16Array -> BufferSource
fromInt16Array = unsafeCoerce   

fromInt32Array :: Int32Array -> BufferSource
fromInt32Array = unsafeCoerce

fromUint8Array :: Uint8Array -> BufferSource
fromUint8Array = unsafeCoerce

fromUint16Array :: Uint16Array -> BufferSource
fromUint16Array = unsafeCoerce  

fromUint32Array :: Uint32Array -> BufferSource
fromUint32Array = unsafeCoerce

fromUint8ClampedArray :: Uint8ClampedArray -> BufferSource
fromUint8ClampedArray = unsafeCoerce

-- todo: get these added to a PS lib
-- fromBigInt64Array :: BigInt64Array -> BufferSource
-- fromBigInt64Array = unsafeCoerce

-- fromBigUint64Array :: BigUint64Array -> BufferSource
-- fromBigUint64Array = unsafeCoerce

fromFloat32Array :: Float32Array -> BufferSource
fromFloat32Array = unsafeCoerce

fromFloat64Array :: Float64Array -> BufferSource
fromFloat64Array = unsafeCoerce

fromDataView :: DataView -> BufferSource
fromDataView = unsafeCoerce
