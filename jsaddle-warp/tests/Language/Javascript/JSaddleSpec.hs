module Language.Javascript.JSaddleSpec where

import Language.Javascript.JSaddle

import qualified Language.Javascript.JSaddle.ObjectSpec as ObjectSpec
import qualified Language.Javascript.JSaddle.ValueSpec as ValueSpec

import Test.Hspec

spec :: SpecWith JSContextRef
spec = do
  describe "ObjectSpec" ObjectSpec.spec
  describe "ValueSpec" ValueSpec.spec
