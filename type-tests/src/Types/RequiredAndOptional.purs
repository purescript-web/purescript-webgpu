module Test.Types.RequiredAndOptiona where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Web.GPU.GPU (requestAdapter)

import Web.GPU.Internal.RequiredAndOptional ((~))
import Web.GPU.Navigator (gpu)
import Web.HTML (window)
import Web.HTML.Window (navigator)

test1 :: Effect Unit
test1 = do
  w <- window
  n <- navigator w
  g' <- gpu n
  for_ g' \g -> do
    _ <- requestAdapter g ({} ~ {})
    pure unit
