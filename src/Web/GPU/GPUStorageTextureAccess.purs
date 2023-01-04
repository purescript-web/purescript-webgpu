module Web.GPU.GPUStorageTextureAccess where

import Prelude

newtype GPUStorageTextureAccess = GPUStorageTextureAccess String

derive instance Eq GPUStorageTextureAccess
derive instance Ord GPUStorageTextureAccess
derive newtype instance Show GPUStorageTextureAccess

writeOnly :: GPUStorageTextureAccess
writeOnly = GPUStorageTextureAccess "write-only"