{-# LANGUAGE ExtendedDefaultRules #-}
module Language.Javascript.JSaddle.ObjectSpec where

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

  describe "(!)" $ do
    it "Lookup a property based on its name." $
      resultShouldBe "11" $ val "Hello World" ! "length"

  describe "(!!)" $ do
    it "Lookup a property based on its index." $
      resultShouldBe "W" $ val "Hello World" !! 6

  describe "js" $ do
    it "Makes a getter for a particular property name." $
      resultShouldBe "11" $ val "Hello World" ^. js "length"

  describe "jsf" $ do
    it "call a function" $
      resultShouldBe "6" $ val "Hello World" ^. jsf "indexOf" ["World"]

  describe "js0" $ do
    it "call a function that expects no arguments" $
      resultShouldBe "hello world" $ val "Hello World" ^. js0 "toLowerCase"

  describe "js1" $ do
    it "call a function that expects one argument" $
      resultShouldBe "6" $ val "Hello World" ^. js1 "indexOf" "World"

  describe "jsgf" $ do
    it "call a function" $
      resultShouldBe "5" $ (eval "globalFunc = function (x) {return x.length;}") >> jsgf "globalFunc" ["World"]

  describe "(#)" $ do
    it "Call a JavaScript function" $
      resultShouldBe "6" $ val "Hello World" # "indexOf" $ ["World"]

  describe "(##)" $ do
    it "Call a JavaScript function at the given index" $
      resultShouldBe "5" $ (eval "something = {}; something[6]=function (x) {return x.length;}; something[6]('World')") >> (jsg "something" ## 6 $ ["World"])

  describe "(<#)" $ do
    it "Set a JavaScript property" $
      resultShouldBe "1" $ do {j <- obj; (j <# "x") 1; j!"x"}

  describe "(<##)" $ do
    it "Set a JavaScript property at the given index" $
      resultShouldBe "1" $ do {j <- obj; (j <## 6) 1; j!!6}

  describe "new" $ do
    it "create a new JavaScript object" $
      resultShouldBe "2013" $ do { a <- new (jsg "Date") (2013, 1, 1); a ^. js0 "getFullYear" }

  describe "call" $ do
    it "Call function with a given @this@." $
      resultShouldBe "Hello" $ do { test <- eval "(function(){return this;})"; call test (val "Hello") () }

  describe "obj" $ do
    it "Make an empty object using the default constuctor" $
      resultShouldBe "Hello" $ do { a <- obj; (a <# "x") "Hello"; a ^. js "x" }

  describe "array" $ do
    it "Make an JavaScript array from a list of values" $
      resultShouldBe "World" $ array ["Hello", "World"] !! 1

    it "Make an JavaScript array from a list of values 2" $
      resultShouldBe "Hello,,,true,1" $ array ("Hello", JSNull, (), True, 1.0::Double)

  describe "propertyNames" $ do
    it "Get a list containing the property names present on a given object" $
      resultShouldBe "[\"x\",\"y\"]" $ show . map strToText <$> propertyNames (eval "({x:1, y:2})")

  describe "nullObject" $ do
    it "is equal to null" $
      resultShouldBe "true" $ strictEqual nullObject (eval "null")
