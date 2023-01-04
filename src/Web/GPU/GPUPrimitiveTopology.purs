module Web.GPU.GPUPrimitiveTopology where

import Prelude

newtype GPUPrimitiveTopology = GPUPrimitiveTopology String

derive instance Eq GPUPrimitiveTopology
derive instance Ord GPUPrimitiveTopology
derive newtype instance Show GPUPrimitiveTopology

pointList :: GPUPrimitiveTopology
pointList = GPUPrimitiveTopology "point-list"

lineList :: GPUPrimitiveTopology
lineList = GPUPrimitiveTopology "line-list"

lineStrip :: GPUPrimitiveTopology
lineStrip = GPUPrimitiveTopology "line-strip"

triangleList :: GPUPrimitiveTopology
triangleList = GPUPrimitiveTopology "triangle-list"

triangleStrip :: GPUPrimitiveTopology
triangleStrip = GPUPrimitiveTopology "triangle-strip"