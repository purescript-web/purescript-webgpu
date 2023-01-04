module Web.GPU.GPUBuffer where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer)
import Effect (Effect)
import Web.GPU.GPUBufferMapState (GPUBufferMapState)
import Web.GPU.GPUBufferUsage (GPUBufferUsage)
import Web.GPU.GPUMapMode (GPUMapMode)
import Web.GPU.Internal.Types (GPUBuffer, GPUSize64)
import Web.Promise (Promise)

foreign import sizeImpl :: GPUBuffer -> Effect GPUSize64

size :: GPUBuffer -> Effect GPUSize64
size = sizeImpl

foreign import usageImpl :: GPUBuffer -> Effect GPUBufferUsage

usage :: GPUBuffer -> Effect GPUBufferUsage
usage = usageImpl

foreign import mapStateImpl :: GPUBuffer -> Effect GPUBufferMapState

mapState :: GPUBuffer -> Effect GPUBufferMapState
mapState = mapStateImpl

foreign import mapAsyncImpl :: GPUBuffer -> GPUMapMode -> Effect (Promise Unit)

mapAsync :: GPUBuffer -> GPUMapMode -> Effect (Promise Unit)
mapAsync = mapAsyncImpl

foreign import mapAsyncWithOffsetImpl
  :: GPUBuffer -> GPUMapMode -> GPUSize64 -> Effect (Promise Unit)

mapAsyncWithOffset
  :: GPUBuffer -> GPUMapMode -> GPUSize64 -> Effect (Promise Unit)
mapAsyncWithOffset = mapAsyncWithOffsetImpl

foreign import mapAsyncWithSizeImpl
  :: GPUBuffer -> GPUMapMode -> GPUSize64 -> Effect (Promise Unit)

mapAsyncWithSize
  :: GPUBuffer -> GPUMapMode -> GPUSize64 -> Effect (Promise Unit)
mapAsyncWithSize = mapAsyncWithSizeImpl

foreign import mapAsyncWithOffsetAndSizeImpl
  :: GPUBuffer -> GPUMapMode -> GPUSize64 -> GPUSize64 -> Effect (Promise Unit)

mapAsyncWithOffsetAndSize
  :: GPUBuffer -> GPUMapMode -> GPUSize64 -> GPUSize64 -> Effect (Promise Unit)
mapAsyncWithOffsetAndSize = mapAsyncWithOffsetAndSizeImpl

foreign import getMappedRangeImpl :: GPUBuffer -> Effect ArrayBuffer

getMappedRange :: GPUBuffer -> Effect ArrayBuffer
getMappedRange = getMappedRangeImpl

foreign import getMappedRangeWithOffsetImpl
  :: GPUBuffer -> GPUSize64 -> Effect ArrayBuffer

getMappedRangeWithOffset :: GPUBuffer -> GPUSize64 -> Effect ArrayBuffer
getMappedRangeWithOffset = getMappedRangeWithOffsetImpl

foreign import getMappedRangeWithSizeImpl
  :: GPUBuffer -> GPUSize64 -> Effect ArrayBuffer

getMappedRangeWithSize :: GPUBuffer -> GPUSize64 -> Effect ArrayBuffer
getMappedRangeWithSize = getMappedRangeWithSizeImpl

foreign import getMappedRangeWithOffsetAndSizeImpl
  :: GPUBuffer -> GPUSize64 -> GPUSize64 -> Effect ArrayBuffer

getMappedRangeWithOffsetAndSize
  :: GPUBuffer -> GPUSize64 -> GPUSize64 -> Effect ArrayBuffer
getMappedRangeWithOffsetAndSize = getMappedRangeWithOffsetAndSizeImpl

foreign import unmapImpl :: GPUBuffer -> Effect Unit

unmap :: GPUBuffer -> Effect Unit
unmap = unmapImpl

foreign import destroyImpl :: GPUBuffer -> Effect Unit

destroy :: GPUBuffer -> Effect Unit
destroy = destroyImpl