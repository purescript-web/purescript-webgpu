module Web.GPU.GPUComputePassTimestampLocation
  ( GPUComputePassTimestampLocation
  , beginning
  , end
  ) where

import Prelude

newtype GPUComputePassTimestampLocation = GPUComputePassTimestampLocation String

derive instance Eq GPUComputePassTimestampLocation
derive instance Ord GPUComputePassTimestampLocation
derive newtype instance Show GPUComputePassTimestampLocation

beginning :: GPUComputePassTimestampLocation
beginning = GPUComputePassTimestampLocation "beginning"

end :: GPUComputePassTimestampLocation
end = GPUComputePassTimestampLocation "end"