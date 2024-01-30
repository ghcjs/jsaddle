{-# LANGUAGE ExtendedDefaultRules #-}
module Language.Javascript.JSaddle.ValueSpec where

import Prelude hiding ((!!))
import Control.Lens.Operators ((^.))

import qualified Data.Text as T
import Language.Javascript.JSaddle
import Test.Hspec

default ( Int)

spec :: SpecWith JSContextRef
spec = do
  let
    resultShouldBe res m ctx = do
      result <- runJSM (valToText =<< m) ctx
      result `shouldBe` (T.pack res)

  describe "valToBool" $ do
    it "JSNull" $
      resultShouldBe "false" $ valToBool JSNull

    it "()" $
      resultShouldBe "false" $ valToBool ()

    it "True" $
      resultShouldBe "true" $ valToBool True

    it "False" $
      resultShouldBe "false" $ valToBool False

    it "(1.0 :: Double)" $
      resultShouldBe "true" $ valToBool (1.0 :: Double)

    it "(0.0 :: Double)" $
      resultShouldBe "false" $ valToBool (0.0 :: Double)

    it "<empty-string>" $
      resultShouldBe "false" $ valToBool ""

    it "1" $
      resultShouldBe "true" $ valToBool 1

  describe "valToNumber" $ do
    it "JSNull" $
      resultShouldBe "0" $ valToNumber JSNull

    -- Throws exception
    xit "()" $
      resultShouldBe "NaN" $ valToNumber ()

    it "True" $
      resultShouldBe "1" $ valToNumber True

    it "False" $
      resultShouldBe "0" $ valToNumber False

    it "(1.0 :: Double)" $
      resultShouldBe "1" $ valToNumber (1.0 :: Double)

    it "(0.0 :: Double)" $
      resultShouldBe "0" $ valToNumber (0.0 :: Double)

    it "<empty-string>" $
      resultShouldBe "0" $ valToNumber ""

    it "1" $
      resultShouldBe "1" $ valToNumber 1

  describe "valToStr" $ do
    it "JSNull" $
      resultShouldBe "null" $ strToText <$> valToStr JSNull

    it "()" $
      resultShouldBe "undefined" $ strToText <$> valToStr ()

    it "True" $
      resultShouldBe "true" $ strToText <$> valToStr True

    it "False" $
      resultShouldBe "false" $ strToText <$> valToStr False

    it "(1.0 :: Double)" $
      resultShouldBe "1" $ strToText <$> valToStr (1.0 :: Double)

    it "(0.0 :: Double)" $
      resultShouldBe "0" $ strToText <$> valToStr (0.0 :: Double)

    it "<empty-string>" $
      resultShouldBe "" $ strToText <$> valToStr ""

    it "1" $
      resultShouldBe "1" $ strToText <$> valToStr 1

  describe "valToJSON" $ do
    it "JSNull" $
      resultShouldBe "null" $ strToText <$> valToJSON JSNull

    it "()" $
      resultShouldBe "" $ strToText <$> valToJSON ()

    it "True" $
      resultShouldBe "true" $ strToText <$> valToJSON True

    it "False" $
      resultShouldBe "false" $ strToText <$> valToJSON False

    it "(1.0 :: Double)" $
      resultShouldBe "1" $ strToText <$> valToJSON (1.0 :: Double)

    it "(0.0 :: Double)" $
      resultShouldBe "0" $ strToText <$> valToJSON (0.0 :: Double)

    it "<empty-string>" $
      resultShouldBe "\"\"" $ strToText <$> valToJSON ""

    it "1" $
      resultShouldBe "1" $ strToText <$> valToJSON 1

    it "<empty-object>" $
      resultShouldBe "{}" $ strToText <$> (valToJSON =<< obj)

  describe "valToObject" $ do
    it "JSNull" $
      resultShouldBe "null" $ valToObject JSNull

    it "()" $
      resultShouldBe "undefined" $ valToObject ()

    it "True" $
      resultShouldBe "true" $ valToObject True

    it "False" $
      resultShouldBe "false" $ valToObject False

    it "(1.0 :: Double)" $
      resultShouldBe "1" $ valToObject (1.0 :: Double)

    it "(0.0 :: Double)" $
      resultShouldBe "0" $ valToObject (0.0 :: Double)

    it "<empty-string>" $
      resultShouldBe "" $ valToObject ""

    it "1" $
      resultShouldBe "1" $ valToObject 1

  describe "strictEqual" $ do
    it "strictEqual True False" $
      resultShouldBe "false" $ strictEqual True False

    it "strictEqual False False" $
      resultShouldBe "true" $ strictEqual False False

    it "strictEqual \"Hello\" ()" $
      resultShouldBe "false" $ strictEqual "Hello" ()

    it "strictEqual \"Hello\" \"Hello\" " $
      resultShouldBe "true" $ strictEqual "Hello" "Hello"

  describe "instanceOf" $ do
    it "Determine if two values are equal (JavaScripts ===)" $
      resultShouldBe "true" $ instanceOf obj (Object <$> jsg "Object")
