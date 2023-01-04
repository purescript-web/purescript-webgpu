module Web.GPU.GPUPowerPreference
  ( GPUPowerPreference(..)
  , highPerformance
  , lowPower
  ) where

import Prelude

newtype GPUPowerPreference = GPUPowerPreference String

derive instance Eq GPUPowerPreference
derive instance Ord GPUPowerPreference
derive newtype instance Show GPUPowerPreference

lowPower ∷ GPUPowerPreference
lowPower = GPUPowerPreference "low-power"

highPerformance ∷ GPUPowerPreference
highPerformance = GPUPowerPreference "high-performance"