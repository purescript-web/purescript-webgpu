module Web.GPU.GPUSamplerBindingType where

import Prelude

newtype GPUSamplerBindingType = GPUSamplerBindingType String

derive instance Eq GPUSamplerBindingType
derive instance Ord GPUSamplerBindingType
derive newtype instance Show GPUSamplerBindingType

filtering :: GPUSamplerBindingType
filtering = GPUSamplerBindingType "filtering"

nonFiltering :: GPUSamplerBindingType
nonFiltering = GPUSamplerBindingType "non-filtering"

comparison :: GPUSamplerBindingType
comparison = GPUSamplerBindingType "comparison"