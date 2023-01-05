module Web.GPU.GPUImageCopyExternalImageSource where

import Unsafe.Coerce (unsafeCoerce)
import Web.GPU.Internal.Types (GPUImageCopyExternalImageSource, ImageBitmap, OffscreenCanvas)
import Web.HTML (HTMLCanvasElement, HTMLVideoElement)

fromImageBitmap :: ImageBitmap -> GPUImageCopyExternalImageSource
fromImageBitmap = unsafeCoerce

fromHTMLVideoElement :: HTMLVideoElement -> GPUImageCopyExternalImageSource
fromHTMLVideoElement = unsafeCoerce

fromHTMLCanvasElement :: HTMLCanvasElement -> GPUImageCopyExternalImageSource
fromHTMLCanvasElement = unsafeCoerce

fromOffscreenCanvas :: OffscreenCanvas -> GPUImageCopyExternalImageSource
fromOffscreenCanvas = unsafeCoerce
