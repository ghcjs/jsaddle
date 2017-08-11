{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-dodgy-imports #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Value
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | Deals with JavaScript values.  These can be
--
--   * null
--
--   * undefined
--
--   * true | false
--
--   * a double precision floating point number
--
--   * a string
--
--   * an object
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Value (
  -- * JavaScript value references
    JSVal
  , ToJSVal(..)

  -- * Haskell types for JavaScript values
  , JSNull(..)
  , JSUndefined
  , JSString
  , JSValue(..)
  , showJSValue

  -- * Converting JavaScript values
  , isTruthy
  , valToBool
  , valToNumber
  , valToStr
  , valToObject
  , valToText
  , valToJSON

  -- * Make JavaScript values from Haskell ones
  , val
  , jsNull
  , valNull
  , isNull
  , valIsNull
  , jsUndefined
  , valUndefined
  , isUndefined
  , valIsUndefined
  , maybeNullOrUndefined
  , maybeNullOrUndefined'
  , toJSBool
  , jsTrue
  , jsFalse
  , valBool
  , valMakeNumber
  , valMakeString
  , valMakeText
  , valMakeJSON

  -- * Convert to and from JSValue
  , deRefVal
  , valMakeRef
  , strictEqual
  , instanceOf
) where

import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Aeson (Value)
import Data.JSString.Text (textToJSString)

#ifdef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Types
       (Object(..), JSString(..), JSVal(..), ghcjsPure)
import GHCJS.Marshal (ToJSVal(..))
#else
import Data.Char (chr, ord)
import Data.Word (Word, Word8, Word16, Word32)
import Data.Int (Int8, Int16, Int32)
import GHCJS.Marshal.Internal (ToJSVal(..), FromJSVal(..))
import Language.Javascript.JSaddle.Types
       (Object(..), JSString(..), JSVal(..), ghcjsPure)
import Language.Javascript.JSaddle.Native
       (valueToNumber, valueToString, valueToJSON, numberToValue, stringToValue, jsonValueToValue)
import qualified Language.Javascript.JSaddle.Native as N
       (deRefVal, strictEqual, instanceOf)
import Language.Javascript.JSaddle.Run (Result(..))
#endif
import Language.Javascript.JSaddle.Monad (JSM)
import Language.Javascript.JSaddle.Classes
       (MakeObject(..), MakeArgs(..))
import Language.Javascript.JSaddle.Marshal.String (ToJSString(..), FromJSString(..))
import Language.Javascript.JSaddle.String (strToText, textToStr)
import GHCJS.Foreign.Internal (jsTrue, jsFalse, jsNull, toJSBool, jsUndefined, isTruthy, isNull, isUndefined)

-- $setup
-- >>> import Language.Javascript.JSaddle.Test (testJSaddle)
-- >>> import Language.Javascript.JSaddle.Monad (catch)
-- >>> import Language.Javascript.JSaddle.Exception (JSException(..))
-- >>> import Language.Javascript.JSaddle.Object (obj, jsg)
-- >>> import qualified Data.Text as T (pack)

data JSNull      = JSNull -- ^ Type that represents a value that can only be null.
                          --   Haskell of course has no null so we are adding this type.
type JSUndefined = ()     -- ^ A type that can only be undefined in JavaScript.  Using ()
                          --   because functions in JavaScript that have no return, impicitly
                          --   return undefined.
-- type JSBool      = Bool   -- ^ JavaScript boolean values map the 'Bool' haskell type.
-- type JSNumber    = Double -- ^ A number in JavaScript maps nicely to 'Double'.
-- type JSString    = Text   -- ^ JavaScript strings can be represented with the Haskell 'Text' type.

-- | An algebraic data type that can represent a JavaScript value.  Any JavaScriptCore
--   'JSVal' can be converted into this type.
data JSValue = ValNull                   -- ^ null
             | ValUndefined              -- ^ undefined
             | ValBool      Bool         -- ^ true or false
             | ValNumber    Double       -- ^ a number
             | ValString    Text         -- ^ a string
             | ValObject    Object       -- ^ an object

-- | Show a JSValue but just say "object" if the value is a JavaScript object.
showJSValue :: JSValue -> String
showJSValue ValNull         = "null"
showJSValue ValUndefined    = "undefined"
showJSValue (ValBool True)  = "true"
showJSValue (ValBool False) = "false"
showJSValue (ValNumber x)   = show x
showJSValue (ValString s)   = show s
showJSValue (ValObject _)   = "object"

-- | Given a JavaScript value get its boolean value.
--   All values in JavaScript convert to bool.
--
-- >>> testJSaddle $ valToBool JSNull
-- false
-- >>> testJSaddle $ valToBool ()
-- false
-- >>> testJSaddle $ valToBool True
-- true
-- >>> testJSaddle $ valToBool False
-- false
-- >>> testJSaddle $ valToBool (1.0 :: Double)
-- true
-- >>> testJSaddle $ valToBool (0.0 :: Double)
-- false
-- >>> testJSaddle $ valToBool ""
-- false
-- >>> testJSaddle $ valToBool "1"
-- true
valToBool :: ToJSVal value => value -> JSM Bool
valToBool value = toJSVal value >>= ghcjsPure . isTruthy

-- | Given a JavaScript value get its numeric value.
--   May throw JSException.
--
-- >>> testJSaddle $ show <$> valToNumber JSNull
-- 0.0
-- >>> testJSaddle $ show <$> valToNumber ()
-- NaN
-- >>> testJSaddle $ show <$> valToNumber True
-- 1.0
-- >>> testJSaddle $ show <$> valToNumber False
-- 0.0
-- >>> testJSaddle $ show <$> valToNumber (1.0 :: Double)
-- 1.0
-- >>> testJSaddle $ show <$> valToNumber (0.0 :: Double)
-- 0.0
-- >>> testJSaddle $ show <$> valToNumber ""
-- 0.0
-- >>> testJSaddle $ show <$> valToNumber "1"
-- 1.0
valToNumber :: ToJSVal value => value -> JSM Double
#ifdef ghcjs_HOST_OS
valToNumber value = jsrefToNumber <$> toJSVal value
foreign import javascript unsafe "$r = Number($1);" jsrefToNumber :: JSVal -> Double
#else
valToNumber value = toJSVal value >>= valueToNumber
#endif

-- | Given a JavaScript value get its string value (as a JavaScript string).
--   May throw JSException.
--
-- >>> testJSaddle $ strToText <$> valToStr JSNull
-- null
-- >>> testJSaddle $ strToText <$> valToStr ()
-- undefined
-- >>> testJSaddle $ strToText <$> valToStr True
-- true
-- >>> testJSaddle $ strToText <$> valToStr False
-- false
-- >>> testJSaddle $ strToText <$> valToStr (1.0 :: Double)
-- 1
-- >>> testJSaddle $ strToText <$> valToStr (0.0 :: Double)
-- 0
-- >>> testJSaddle $ strToText <$> valToStr ""
-- <BLANKLINE>
-- >>> testJSaddle $ strToText <$> valToStr "1"
-- 1
valToStr :: ToJSVal value => value -> JSM JSString
#ifdef ghcjs_HOST_OS
valToStr value = jsrefToString <$> toJSVal value
foreign import javascript unsafe "$r = $1.toString();" jsrefToString :: JSVal -> JSString
#else
valToStr value = toJSVal value >>= valueToString
#endif

-- | Given a JavaScript value get its string value (as a Haskell 'Text').
--   May throw JSException.
--
-- >>> testJSaddle $ show <$> valToText JSNull
-- "null"
-- >>> testJSaddle $ show <$> valToText ()
-- "undefined"
-- >>> testJSaddle $ show <$> valToText True
-- "true"
-- >>> testJSaddle $ show <$> valToText False
-- "false"
-- >>> testJSaddle $ show <$> valToText (1.0 :: Double)
-- "1"
-- >>> testJSaddle $ show <$> valToText (0.0 :: Double)
-- "0"
-- >>> testJSaddle $ show <$> valToText ""
-- ""
-- >>> testJSaddle $ show <$> valToText "1"
-- "1"
valToText :: ToJSVal value => value -> JSM Text
valToText jsvar = strToText <$> valToStr jsvar

-- | Given a JavaScript value get a JSON string value.
--   May throw JSException.
--
-- >>> testJSaddle $ strToText <$> valToJSON JSNull
-- null
-- >>> testJSaddle $ strToText <$> valToJSON ()
-- <BLANKLINE>
-- >>> testJSaddle $ strToText <$> valToJSON True
-- true
-- >>> testJSaddle $ strToText <$> valToJSON False
-- false
-- >>> testJSaddle $ strToText <$> valToJSON (1.0 :: Double)
-- 1
-- >>> testJSaddle $ strToText <$> valToJSON (0.0 :: Double)
-- 0
-- >>> testJSaddle $ strToText <$> valToJSON ""
-- ""
-- >>> testJSaddle $ strToText <$> valToJSON "1"
-- "1"
-- >>> testJSaddle $ strToText <$> (obj >>= valToJSON)
-- {}
valToJSON :: ToJSVal value => value -> JSM JSString
#ifdef ghcjs_HOST_OS
valToJSON value = jsrefToJSON <$> toJSVal value
foreign import javascript unsafe "$r = $1 === undefined ? \"\" : JSON.stringify($1);" jsrefToJSON :: JSVal -> JSString
#else
valToJSON value = toJSVal value >>= valueToJSON
#endif

-- | Given a JavaScript value get its object value.
--   May throw JSException.
--
-- >>> testJSaddle $ (valToObject JSNull >>= valToText) `catch` \ (JSException e) -> valToText e
-- null
-- >>> testJSaddle $ (valToObject () >>= valToText) `catch` \ (JSException e) -> valToText e
-- undefined
-- >>> testJSaddle $ valToObject True
-- true
-- >>> testJSaddle $ valToObject False
-- false
-- >>> testJSaddle $ valToObject (1.0 :: Double)
-- 1
-- >>> testJSaddle $ valToObject (0.0 :: Double)
-- 0
-- >>> testJSaddle $ valToObject ""
-- <BLANKLINE>
-- >>> testJSaddle $ valToObject "1"
-- 1
valToObject :: ToJSVal value => value -> JSM Object
valToObject value = Object <$> toJSVal value

instance MakeObject JSVal where
    makeObject = return . Object

instance ToJSVal Object where
    toJSVal (Object r) = return r

-- | Convert to a JavaScript value (just an alias for 'toJSVal')
val :: ToJSVal value
    => value          -- ^ value to convert to a JavaScript value
    -> JSM JSVal
val = toJSVal

#ifndef ghcjs_HOST_OS
-- | If we already have a JSVal we are fine
instance ToJSVal JSVal where
    toJSVal = return
    {-# INLINE toJSVal #-}
#endif

-- | A single JSVal can be used as the argument list
instance MakeArgs JSVal where
    makeArgs arg = return [arg]

-- | JSVal can be made by evaluating a function in 'JSM' as long
--   as it returns something we can make into a JSVal.
instance ToJSVal v => ToJSVal (JSM v) where
    toJSVal v = v >>= toJSVal
    {-# INLINE toJSVal #-}

----------- null ---------------
-- | A @null@ JavaScript value
valNull :: JSVal
valNull = jsNull
{-# INLINE valNull #-}

-- | Makes a @null@ JavaScript value
instance ToJSVal JSNull where
    toJSVal = const (return jsNull)
    {-# INLINE toJSVal #-}

-- | Makes an argument list with just a single @null@ JavaScript value
instance MakeArgs JSNull where
    makeArgs _ = return [jsNull]

#ifndef ghcjs_HOST_OS
-- | Makes a JSVal or @null@ JavaScript value
instance ToJSVal a => ToJSVal (Maybe a) where
    toJSVal Nothing = return jsNull
    toJSVal (Just a) = toJSVal a
    {-# INLINE toJSVal #-}
instance FromJSVal a => FromJSVal (Maybe a) where
    fromJSValUnchecked x =
        ghcjsPure (isUndefined x) >>= \case
            True  -> return Nothing
            False -> ghcjsPure (isNull x) >>= \case
                    True  -> return Nothing
                    False -> fromJSVal x
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal x =
        ghcjsPure (isUndefined x) >>= \case
            True  -> return (Just Nothing)
            False -> ghcjsPure (isNull x) >>= \case
                    True  -> return (Just Nothing)
                    False -> fmap (fmap Just) fromJSVal x
    {-# INLINE fromJSVal #-}

-- Make an array out of various lists
instance ToJSVal a => ToJSVal [a] where
    toJSVal = toJSValListOf
    {-# INLINE toJSVal #-}

instance FromJSVal a => FromJSVal [a] where
    fromJSVal = fromJSValListOf
    {-# INLINE fromJSVal #-}
#endif

-- | Test a JavaScript value to see if it is @null@
valIsNull :: ToJSVal value => value -> JSM Bool
valIsNull value = toJSVal value >>= ghcjsPure . isNull

----------- undefined ---------------
-- | An @undefined@ JavaScript value
valUndefined :: JSVal
valUndefined = jsUndefined
{-# INLINE valUndefined #-}

-- | Makes an @undefined@ JavaScript value
instance ToJSVal JSUndefined where
    toJSVal = const (return jsUndefined)

--We can't allow this if JSUndefined is () as () is no args not "(null)".
--Use [()] instead.
--instance MakeArgs JSUndefined where
--    makeArgs _ = valMakeUndefined >>= (\ref -> return [ref])

-- | This allows us to pass no arguments easily (altenative would be to use @[]::[JSVal]@).
instance MakeArgs () where
    makeArgs _ = return []

-- | Test a JavaScript value to see if it is @undefined@
valIsUndefined :: ToJSVal value => value -> JSM Bool
valIsUndefined value = toJSVal value >>= ghcjsPure . isUndefined

-- | Convert a JSVal to a Maybe JSVal (converting null and undefined to Nothing)
maybeNullOrUndefined :: ToJSVal value => value -> JSM (Maybe JSVal)
maybeNullOrUndefined value = do
    rval <- toJSVal value
    ghcjsPure (isNull rval) >>= \case
        True -> return Nothing
        _    ->
            ghcjsPure (isUndefined rval) >>= \case
                True -> return Nothing
                _    -> return (Just rval)

maybeNullOrUndefined' :: ToJSVal value => (JSVal -> JSM a) -> value -> JSM (Maybe a)
maybeNullOrUndefined' f value = do
    rval <- toJSVal value
    ghcjsPure (isNull rval) >>= \case
        True -> return Nothing
        _    ->
            ghcjsPure (isUndefined rval) >>= \case
                True -> return Nothing
                _    -> Just <$> f rval

----------- booleans ---------------
-- | A JavaScript boolean value
valBool :: Bool -> JSVal
valBool = toJSBool
{-# INLINE valBool #-}

#ifndef ghcjs_HOST_OS
-- | Make a JavaScript boolean value
instance ToJSVal Bool where
    toJSVal = return . valBool
    {-# INLINE toJSVal #-}
#endif

-- | Makes an argument list with just a single JavaScript boolean value
instance MakeArgs Bool where
    makeArgs b = return [valBool b]

----------- numbers ---------------
-- | Make a JavaScript number
valMakeNumber :: Double -> JSM JSVal
valMakeNumber = toJSVal
{-# INLINE valMakeNumber #-}

#ifndef ghcjs_HOST_OS
-- | Makes a JavaScript number
instance ToJSVal Double where
    toJSVal = numberToValue
    {-# INLINE toJSVal #-}

instance ToJSVal Float where
    toJSVal = numberToValue . realToFrac
    {-# INLINE toJSVal #-}

instance ToJSVal Word where
    toJSVal = numberToValue . fromIntegral
    {-# INLINE toJSVal #-}

instance ToJSVal Word8 where
    toJSVal = numberToValue . fromIntegral
    {-# INLINE toJSVal #-}

instance ToJSVal Word16 where
    toJSVal = numberToValue . fromIntegral
    {-# INLINE toJSVal #-}

instance ToJSVal Word32 where
    toJSVal = numberToValue . fromIntegral
    {-# INLINE toJSVal #-}

instance ToJSVal Int where
    toJSVal = numberToValue . fromIntegral
    {-# INLINE toJSVal #-}

instance ToJSVal Int8 where
    toJSVal = numberToValue . fromIntegral
    {-# INLINE toJSVal #-}

instance ToJSVal Int16 where
    toJSVal = numberToValue . fromIntegral
    {-# INLINE toJSVal #-}

instance ToJSVal Int32 where
    toJSVal = numberToValue . fromIntegral
    {-# INLINE toJSVal #-}
#endif

-- | Makes an argument list with just a single JavaScript number
instance MakeArgs Double where
    makeArgs n = valMakeNumber n >>= (\ref -> return [ref])

-- | Make a JavaScript string from `Text`
valMakeText :: Text -> JSM JSVal
valMakeText = toJSVal . textToJSString
{-# INLINE valMakeText #-}

-- | Make a JavaScript string from `JSString`
valMakeString :: JSString -> JSM JSVal
valMakeString = toJSVal
{-# INLINE valMakeString #-}

#ifndef ghcjs_HOST_OS
-- | Makes a JavaScript string
instance ToJSVal Text where
    toJSVal = stringToValue . JSString
    {-# INLINE toJSVal #-}
instance FromJSVal Text where
    fromJSValUnchecked = valToText
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap Just . valToText
    {-# INLINE fromJSVal #-}
#endif

-- | Makes an argument list with just a single JavaScript string
instance MakeArgs Text where
    makeArgs t = valMakeText t >>= (\ref -> return [ref])

#ifndef ghcjs_HOST_OS
-- | Makes a JavaScript string
instance ToJSVal JSString where
    toJSVal = stringToValue
    {-# INLINE toJSVal #-}
instance FromJSVal JSString where
    fromJSValUnchecked = valToStr
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap Just . valToStr
    {-# INLINE fromJSVal #-}
#endif

-- | If we already have a JSString we are fine
instance ToJSString JSString where
    toJSString = id

instance ToJSString Text where
    toJSString = textToStr

instance ToJSString String where
    toJSString = textToStr . T.pack

instance FromJSString Text where
    fromJSString = strToText

instance FromJSString String where
    fromJSString v = T.unpack $ strToText v

instance FromJSString JSString where
    fromJSString = id

#ifndef ghcjs_HOST_OS
instance ToJSVal Char where
    toJSVal = valMakeNumber . fromIntegral . ord
    {-# INLINE toJSVal #-}
    toJSValListOf = valMakeText . T.pack
    {-# INLINE toJSValListOf #-}
instance FromJSVal Char where
    fromJSValUnchecked = fmap (chr . round) . valToNumber
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap (Just . chr . round) . valToNumber
    {-# INLINE fromJSVal #-}
    fromJSValUncheckedListOf = fmap (T.unpack . strToText) . valToStr
    {-# INLINE fromJSValListOf #-}
    fromJSValListOf = fmap (Just . T.unpack . strToText) . valToStr
    {-# INLINE fromJSValUncheckedListOf #-}
#endif

-- | Make a JavaScript string from AESON `Value`
valMakeJSON :: Value -> JSM JSVal
valMakeJSON = toJSVal

#ifndef ghcjs_HOST_OS
-- | Makes a JSON value
instance ToJSVal Value where
    toJSVal = jsonValueToValue
    {-# INLINE toJSVal #-}
#endif

-- | Makes an argument list with just a single JSON value
instance MakeArgs Value where
    makeArgs t = valMakeJSON t >>= (\ref -> return [ref])

-- | Derefernce a value reference.
--
-- >>> testJSaddle $ showJSValue <$> deRefVal JSNull
-- null
-- >>> testJSaddle $ showJSValue <$> deRefVal ()
-- undefined
-- >>> testJSaddle $ showJSValue <$> deRefVal True
-- true
-- >>> testJSaddle $ showJSValue <$> deRefVal False
-- false
-- >>> testJSaddle $ showJSValue <$> deRefVal (1.0 :: Double)
-- 1.0
-- >>> testJSaddle $ showJSValue <$> deRefVal (0.0 :: Double)
-- 0.0
-- >>> testJSaddle $ showJSValue <$> deRefVal ""
-- ""
-- >>> testJSaddle $ showJSValue <$> deRefVal "1"
-- "1"
-- >>> testJSaddle $ showJSValue <$> (valToObject True >>= deRefVal)
-- true
-- >>> testJSaddle $ showJSValue <$> (obj >>= deRefVal)
-- object
deRefVal :: ToJSVal value => value -> JSM JSValue
#ifdef ghcjs_HOST_OS
deRefVal value = do
    valref <- toJSVal value
    case (jsrefGetType valref :: Int) of
        0 -> return ValUndefined
        1 -> return ValNull
        2 -> ValBool   <$> valToBool valref
        3 -> ValNumber <$> valToNumber valref
        4 -> ValString <$> valToText valref
        5 -> ValObject <$> valToObject valref
        _ -> error "Unexpected result dereferencing JSaddle value"
foreign import javascript unsafe "$r = ($1 === undefined)?0:\
                                       ($1===null)?1:\
                                       (typeof $1===\"boolean\")?2:\
                                       (typeof $1===\"number\")?3:\
                                       (typeof $1===\"string\")?4:\
                                       (typeof $1===\"object\")?5:-1;" jsrefGetType :: JSVal -> Int
#else
deRefVal value = do
    v <- toJSVal value
    result <- N.deRefVal v
    return $ case result of
        DeRefValResult 0    _ -> ValNull
        DeRefValResult 1    _ -> ValUndefined
        DeRefValResult 2    _ -> ValBool False
        DeRefValResult 3    _ -> ValBool True
        DeRefValResult (-1) s -> ValNumber (read (T.unpack s))
        DeRefValResult (-2) s -> ValString s
        DeRefValResult (-3) _ -> ValObject (Object v)
        _                     -> error "Unexpected result dereferencing JSaddle value"
#endif

-- | Make a JavaScript value out of a 'JSValue' ADT.
--
-- >>> testJSaddle $ valMakeRef ValNull
-- null
-- >>> testJSaddle $ valMakeRef ValUndefined
-- undefined
-- >>> testJSaddle $ valMakeRef (ValBool True)
-- true
-- >>> testJSaddle $ valMakeRef (ValNumber 1)
-- 1
-- >>> testJSaddle $ valMakeRef (ValString $ T.pack "Hello")
-- Hello
valMakeRef :: JSValue -> JSM JSVal
valMakeRef value =
    case value of
        ValNull              -> return valNull
        ValUndefined         -> return valUndefined
        ValBool b            -> return $ valBool b
        ValNumber n          -> valMakeNumber n
        ValString s          -> valMakeText s
        ValObject (Object o) -> return o

-- | Makes a JavaScript value from a 'JSValue' ADT.
instance ToJSVal JSValue where
    toJSVal = valMakeRef
    {-# INLINE toJSVal #-}

-- | Makes an argument list with just a single JavaScript value from a 'JSValue' ADT.
instance MakeArgs JSValue where
    makeArgs v = valMakeRef v >>= (\ref -> return [ref])

--instance MakeObjectRef JSNull where
--    makeObjectRef _ = Object <$> valMakeNull
--    {-# INLINE makeObjectRef #-}

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe
  "$1===$2" jsvalueisstrictequal :: JSVal -> JSVal -> Bool
#endif

-- | Determine if two values are equal (JavaScripts ===)
-- >>> testJSaddle $ strictEqual True False
-- false
-- >>> testJSaddle $ strictEqual True True
-- true
-- >>> testJSaddle $ strictEqual "Hello" ()
-- false
-- >>> testJSaddle $ strictEqual "Hello" "Hello"
-- true
strictEqual :: (ToJSVal a, ToJSVal b) => a -> b -> JSM Bool
strictEqual a b = do
    aval <- toJSVal a
    bval <- toJSVal b
#ifdef ghcjs_HOST_OS
    return $ jsvalueisstrictequal aval bval
#else
    N.strictEqual aval bval
#endif

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$1 instanceof $2"
  js_isInstanceOf :: JSVal -> Object -> Bool
#endif

-- | Determine if two values are equal (JavaScripts ===)
-- >>> testJSaddle $ instanceOf obj (Object <$> jsg "Object")
-- true
instanceOf :: (ToJSVal value, MakeObject constructor) => value -> constructor -> JSM Bool
instanceOf value constructor = do
    v <- toJSVal value
    c <- makeObject constructor
#ifdef ghcjs_HOST_OS
    return $ js_isInstanceOf v c
#else
    N.instanceOf v c
#endif











