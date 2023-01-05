module Web.GPU.UnmaskHint
  ( UnmaskHint
  , architecture
  , description
  , device
  , vendor
  ) where

import Prelude

newtype UnmaskHint = UnmaskHint String
vendor = UnmaskHint "vendor" :: UnmaskHint
architecture = UnmaskHint "architecture" :: UnmaskHint
device = UnmaskHint "device" :: UnmaskHint
description = UnmaskHint "description" :: UnmaskHint

derive instance Eq UnmaskHint
derive instance Ord UnmaskHint
derive newtype instance Show UnmaskHint