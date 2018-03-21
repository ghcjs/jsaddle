{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Native
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Native.Internal (
    setPropertyByName
  , setPropertyAtIndex
  , stringToValue
  , numberToValue
  , jsonValueToValue
  , getPropertyByName
  , getPropertyAtIndex
  , callAsFunction
  , callAsConstructor
  , newEmptyObject
  , newAsyncCallback
  , newSyncCallback
  , newArray
  , evaluateScript
  , valueToBool
  , valueToNumber
  , valueToString
  , valueToJSON
  , valueToJSONValue
  , isNull
  , isUndefined
  , strictEqual
  , instanceOf
  , propertyNames
) where

import Data.Aeson (Value)
import qualified Data.Aeson as A
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as LBS

import GHCJS.Prim.Internal
import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Run

--TODO: Make JSString a JSVal under the hood
setPropertyByName :: JSString -> JSVal -> Object -> JSM ()
setPropertyByName (JSString name) val (Object this) = setProperty (primToJSVal $ PrimVal_String name) val this
{-# INLINE setPropertyByName #-}

setPropertyAtIndex :: Int -> JSVal -> Object -> JSM ()
setPropertyAtIndex index val (Object this) = setProperty (primToJSVal $ PrimVal_Number $ fromIntegral index) val this
{-# INLINE setPropertyAtIndex #-}

stringToValue :: JSString -> JSM JSVal
stringToValue = return . primToJSVal . PrimVal_String . unJSString
{-# INLINE stringToValue #-}

numberToValue :: Double -> JSM JSVal
numberToValue = return . primToJSVal . PrimVal_Number
{-# INLINE numberToValue #-}

jsonValueToValue :: Value -> JSM JSVal
jsonValueToValue = newJson
{-# INLINE jsonValueToValue #-}

getPropertyByName :: JSString -> Object -> JSM JSVal
getPropertyByName (JSString name) (Object this) = getProperty (primToJSVal $ PrimVal_String name) this
{-# INLINE getPropertyByName #-}

getPropertyAtIndex :: Int -> Object -> JSM JSVal
getPropertyAtIndex index (Object this) = getProperty (primToJSVal $ PrimVal_Number $ fromIntegral index) this
{-# INLINE getPropertyAtIndex #-}

callAsFunction :: Object -> Object -> [JSVal] -> JSM JSVal
callAsFunction (Object f) (Object this) args = callAsFunction' f this args
{-# INLINE callAsFunction #-}

callAsConstructor :: Object -> [JSVal] -> JSM JSVal
callAsConstructor (Object f) args = callAsConstructor' f args
{-# INLINE callAsConstructor #-}

newEmptyObject :: JSM Object
newEmptyObject = Object <$> newJson (A.Object mempty)
{-# INLINE newEmptyObject #-}

newAsyncCallback :: JSCallAsFunction -> JSM (CallbackId, JSVal)
newAsyncCallback = newSyncCallback'
{-# INLINE newAsyncCallback #-}

newSyncCallback :: JSCallAsFunction -> JSM (CallbackId, JSVal)
newSyncCallback = newSyncCallback'
{-# INLINE newSyncCallback #-}

getGlobal :: JSString -> JSM JSVal
getGlobal (JSString name) = do
  getProperty (primToJSVal $ PrimVal_String name) $ primToJSVal $ PrimVal_Ref globalRef

newArray :: [JSVal] -> JSM JSVal
newArray xs = do
  array <- getGlobal "Array"
  callAsConstructor' array xs
{-# INLINE newArray #-}

evaluateScript :: JSString -> JSM JSVal
evaluateScript (JSString str) = do
  eval <- getGlobal "eval"
  callAsFunction' eval (primToJSVal $ PrimVal_Ref globalRef) [primToJSVal $ PrimVal_String str]
{-# INLINE evaluateScript #-}

valueToBool :: JSVal -> JSM Bool
valueToBool val = case getPrimJSVal val of
  PrimVal_Undefined -> return False
  PrimVal_Null -> return False
  PrimVal_Bool b -> return b
  PrimVal_Number n -> return $ n /= 0 --TODO: NaN, although it should come across as "null", so this should work accidentally
  PrimVal_String s -> return $ s /= ""
  PrimVal_Ref _ -> return True -- Always truthy, because all falsey values are primitive
  -- I think what we want to do is:
  --   Whenever the JS side fills in a reference for us, it automatically sends a response saying what it filled it in with - a PrimVal (), which represents whether it was filled in with a primitive value or with something else.  Until that response arrives, we can keep using the reference value; once it arrives, we can finalize our reference (sending the FreeVal command) and then simply send the simple value.
{-# INLINE valueToBool #-}

valueToNumber :: JSVal -> JSM Double
valueToNumber val = case getPrimJSVal val of
  PrimVal_Undefined -> return $ 0/0 -- NaN
  PrimVal_Null -> return 0
  PrimVal_Bool False -> return 0
  PrimVal_Bool True -> return 1
  PrimVal_Number n -> return n
  PrimVal_String _ -> do --TODO: Race: by forcing the value first, we're adding some latency in the slow case; perhaps we want to race the two options instead
    number <- getGlobal "Number"
    valueToNumber =<< callAsFunction' number number [val]
  PrimVal_Ref _ -> do --TODO: Race
    number <- getGlobal "Number"
    valueToNumber =<< callAsFunction' number number [val]
{-# INLINE valueToNumber #-}

valueToString :: JSVal -> JSM JSString
valueToString val = case getPrimJSVal val of
  PrimVal_Undefined -> return "undefined"
  PrimVal_Null -> return "null"
  PrimVal_Bool False -> return "false"
  PrimVal_Bool True -> return "true"
  PrimVal_Number _ -> do --TODO: Race
    toString <- getPropertyByName "toString" $ Object val
    valueToString =<< callAsFunction' toString val []
  PrimVal_String s -> return $ JSString s
  PrimVal_Ref _ -> do
    toString <- getPropertyByName "toString" $ Object val
    valueToString =<< callAsFunction' toString val []
{-# INLINE valueToString #-}

valueToJSON :: JSVal -> JSM JSString
valueToJSON value = do
  JSString . decodeUtf8 . LBS.toStrict . A.encode <$> getJsonLazy value
{-# INLINE valueToJSON #-}

valueToJSONValue :: JSVal -> JSM Value
valueToJSONValue = getJsonLazy
{-# INLINE valueToJSONValue #-}

isNull :: JSVal -> JSM Bool
isNull val = case getPrimJSVal val of
  PrimVal_Null -> return True
  _ -> return False
{-# INLINE isNull #-}

isUndefined :: JSVal -> JSM Bool
isUndefined val = case getPrimJSVal val of
  PrimVal_Undefined -> return True
  _ -> return False
{-# INLINE isUndefined #-}

strictEqual :: JSVal -> JSVal -> JSM Bool
strictEqual a b = do
  fun <- evaluateScript "(function(a,b){return a===b;})"
  valueToBool =<< callAsFunction (Object fun) (Object jsNull) [a, b]
{-# INLINE strictEqual #-}

instanceOf :: JSVal -> Object -> JSM Bool
instanceOf value (Object constructor) = do
  fun <- evaluateScript "(function(a,b){return a instanceof b;})"
  valueToBool =<< callAsFunction (Object fun) (Object jsNull) [value, constructor]
{-# INLINE instanceOf #-}

propertyNames :: Object -> JSM [JSString]
propertyNames (Object this) = do
  fun <- evaluateScript "(function(a){var r = []; for(n in a) { r.push(n); } return r;})"
  val <- valueToJSONValue =<< callAsFunction (Object fun) (Object jsNull) [this]
  return $ case A.fromJSON val of
    A.Success a -> a
    A.Error f -> error $ "propertyNames: " ++ f
{-# INLINE propertyNames #-}
