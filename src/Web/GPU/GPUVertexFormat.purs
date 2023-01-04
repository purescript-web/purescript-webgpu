module Web.GPU.GPUVertexFormat
  ( GPUVertexFormat
  , float16x2
  , float16x4
  , float32
  , float32x2
  , float32x3
  , float32x4
  , sint16x2
  , sint16x4
  , sint32
  , sint32x2
  , sint32x3
  , sint32x4
  , sint8x2
  , sint8x4
  , snorm16x2
  , snorm16x4
  , snorm8x2
  , snorm8x4
  , uint16x2
  , uint16x4
  , uint32
  , uint32x2
  , uint32x3
  , uint32x4
  , uint8x2
  , uint8x4
  , unorm16x2
  , unorm16x4
  , unorm8x2
  , unorm8x4
  ) where

import Prelude

newtype GPUVertexFormat = GPUVertexFormat String

derive instance Eq GPUVertexFormat
derive instance Ord GPUVertexFormat
derive newtype instance Show GPUVertexFormat

uint8x2 = GPUVertexFormat "uint8x2" :: GPUVertexFormat
uint8x4 = GPUVertexFormat "uint8x4" :: GPUVertexFormat
sint8x2 = GPUVertexFormat "sint8x2" :: GPUVertexFormat
sint8x4 = GPUVertexFormat "sint8x4" :: GPUVertexFormat
unorm8x2 = GPUVertexFormat "unorm8x2" :: GPUVertexFormat
unorm8x4 = GPUVertexFormat "unorm8x4" :: GPUVertexFormat
snorm8x2 = GPUVertexFormat "snorm8x2" :: GPUVertexFormat
snorm8x4 = GPUVertexFormat "snorm8x4" :: GPUVertexFormat
uint16x2 = GPUVertexFormat "uint16x2" :: GPUVertexFormat
uint16x4 = GPUVertexFormat "uint16x4" :: GPUVertexFormat
sint16x2 = GPUVertexFormat "sint16x2" :: GPUVertexFormat
sint16x4 = GPUVertexFormat "sint16x4" :: GPUVertexFormat
unorm16x2 = GPUVertexFormat "unorm16x2" :: GPUVertexFormat
unorm16x4 = GPUVertexFormat "unorm16x4" :: GPUVertexFormat
snorm16x2 = GPUVertexFormat "snorm16x2" :: GPUVertexFormat
snorm16x4 = GPUVertexFormat "snorm16x4" :: GPUVertexFormat
float16x2 = GPUVertexFormat "float16x2" :: GPUVertexFormat
float16x4 = GPUVertexFormat "float16x4" :: GPUVertexFormat
float32 = GPUVertexFormat "float32" :: GPUVertexFormat
float32x2 = GPUVertexFormat "float32x2" :: GPUVertexFormat
float32x3 = GPUVertexFormat "float32x3" :: GPUVertexFormat
float32x4 = GPUVertexFormat "float32x4" :: GPUVertexFormat
uint32 = GPUVertexFormat "uint32" :: GPUVertexFormat
uint32x2 = GPUVertexFormat "uint32x2" :: GPUVertexFormat
uint32x3 = GPUVertexFormat "uint32x3" :: GPUVertexFormat
uint32x4 = GPUVertexFormat "uint32x4" :: GPUVertexFormat
sint32 = GPUVertexFormat "sint32" :: GPUVertexFormat
sint32x2 = GPUVertexFormat "sint32x2" :: GPUVertexFormat
sint32x3 = GPUVertexFormat "sint32x3" :: GPUVertexFormat
sint32x4 = GPUVertexFormat "sint32x4" :: GPUVertexFormat