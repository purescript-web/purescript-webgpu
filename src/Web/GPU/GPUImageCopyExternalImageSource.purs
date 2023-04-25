-- @inline export fromImageBitmap arity=1
-- @inline export fromHTMLVideoElement arity=1
-- @inline export fromHTMLCanvasElement arity=1
-- @inline export fromOffscreenCanvas arity=1
module Web.GPU.GPUImageCopyExternalImageSource where

import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.Types (ImageBitmap, OffscreenCanvas)
import Web.HTML (HTMLCanvasElement, HTMLVideoElement)

data GPUImageCopyExternalImageSource

fromImageBitmap :: ImageBitmap -> GPUImageCopyExternalImageSource
fromImageBitmap = unsafeCoerce

fromHTMLVideoElement :: HTMLVideoElement -> GPUImageCopyExternalImageSource
fromHTMLVideoElement = unsafeCoerce

fromHTMLCanvasElement :: HTMLCanvasElement -> GPUImageCopyExternalImageSource
fromHTMLCanvasElement = unsafeCoerce

fromOffscreenCanvas :: OffscreenCanvas -> GPUImageCopyExternalImageSource
fromOffscreenCanvas = unsafeCoerce