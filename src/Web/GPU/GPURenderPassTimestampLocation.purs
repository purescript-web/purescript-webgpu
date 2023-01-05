module Web.GPU.GPURenderPassTimestampLocation
  ( GPURenderPassTimestampLocation
  , beginning
  , end
  ) where

import Prelude

newtype GPURenderPassTimestampLocation = GPURenderPassTimestampLocation String

derive instance Eq GPURenderPassTimestampLocation
derive instance Ord GPURenderPassTimestampLocation
derive newtype instance Show GPURenderPassTimestampLocation

beginning :: String
beginning = "beginning"

end :: String
end = "end"