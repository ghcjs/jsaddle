{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
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
  , valToBool
  , valToNumber
  , valToStr
  , valToObject
  , valToText
  , valToJSON

  -- * Make JavaScript values from Haskell ones
  , val
  , valNull
  , valIsNull
  , valUndefined
  , valIsUndefined
  , maybeNullOrUndefined
  , maybeNullOrUndefined'
  , valBool
  , valMakeNumber
  , valString

  -- * Convert to and from JSValue
  , deRefVal
  , valMakeRef
  , strictEqual
  , instanceOf
) where

import Control.Applicative
import Prelude hiding (catch)
import Language.Javascript.JSaddle.Types
       (Object(..), JSString(..), JSVal(..))
#ifdef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Types
       (MutableJSArray)
import GHCJS.Types (JSVal(..), isNull, isUndefined)
import GHCJS.Foreign (toJSBool, isTruthy, jsNull, jsUndefined)
import qualified GHCJS.Marshal as GHCJS (toJSVal)
import GHCJS.Marshal.Pure (pToJSVal)
import Data.JSString.Text (textToJSString)
import Language.Javascript.JSaddle.Exception (rethrow)
#else
import Language.Javascript.JSaddle.Native
       (wrapJSString, withJSVal, withObject, withJSString,
        withToJSVal, wrapJSVal)
import Language.Javascript.JSaddle.WebSockets (Command(..), Result(..), sendCommand)
#endif
import Language.Javascript.JSaddle.Monad (JSM)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Language.Javascript.JSaddle.Classes
       (MakeObject(..), ToJSString(..), ToJSVal(..))
import Language.Javascript.JSaddle.String (strToText, textToStr)
import Language.Javascript.JSaddle.Arguments (MakeArgs(..))
import Data.Word (Word32, Word, Word64)
import Data.Int (Int32, Int64)

-- $setup
-- >>> import Language.Javascript.JSaddle.Test (testJSaddle)
-- >>> import Language.Javascript.JSaddle.Monad (catch)
-- >>> import Language.Javascript.JSaddle.Exception (JSException(..))
-- >>> import Language.Javascript.JSaddle.Object (obj)
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
#ifdef ghcjs_HOST_OS
valToBool value = isTruthy <$> toJSVal value
#else
valToBool value =
    toJSVal value >>= \case
        (JSVal 0) -> return False -- null
        (JSVal 1) -> return False -- undefined
        (JSVal 2) -> return False -- false
        (JSVal 3) -> return True  -- true
        val ->
            withJSVal val $ \rval -> do
                ValueToBoolResult result <- sendCommand (ValueToBool rval)
                return result
#endif

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
valToNumber value =
    withToJSVal value $ \rval -> do
        ValueToNumberResult result <- sendCommand (ValueToNumber rval)
        return result
#endif

-- | Given a JavaScript value get its string value (as a JavaScript string).
--   May throw JSException.
--
-- >>> testJSaddle $ valToStr JSNull >>= strToText
-- null
-- >>> testJSaddle $ valToStr () >>= strToText
-- undefined
-- >>> testJSaddle $ valToStr True >>= strToText
-- true
-- >>> testJSaddle $ valToStr False >>= strToText
-- false
-- >>> testJSaddle $ valToStr (1.0 :: Double) >>= strToText
-- 1
-- >>> testJSaddle $ valToStr (0.0 :: Double) >>= strToText
-- 0
-- >>> testJSaddle $ valToStr "" >>= strToText
-- <BLANKLINE>
-- >>> testJSaddle $ valToStr "1" >>= strToText
-- 1
valToStr :: ToJSVal value => value -> JSM JSString
#ifdef ghcjs_HOST_OS
valToStr value = jsrefToString <$> toJSVal value
foreign import javascript unsafe "$r = $1.toString();" jsrefToString :: JSVal -> JSString
#else
valToStr value =
    withToJSVal value $ \rval -> do
        ValueToStringResult result <- sendCommand (ValueToString rval)
        wrapJSString result
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
valToText jsvar = valToStr jsvar >>= strToText

-- | Given a JavaScript value get a JSON string value.
--   May throw JSException.
--
-- >>> testJSaddle $ valToJSON 0 JSNull >>= strToText
-- null
-- >>> testJSaddle $ valToJSON 0 () >>= strToText
-- <BLANKLINE>
-- >>> testJSaddle $ valToJSON 0 True >>= strToText
-- true
-- >>> testJSaddle $ valToJSON 0 False >>= strToText
-- false
-- >>> testJSaddle $ valToJSON 0 (1.0 :: Double) >>= strToText
-- 1
-- >>> testJSaddle $ valToJSON 0 (0.0 :: Double) >>= strToText
-- 0
-- >>> testJSaddle $ valToJSON 0 "" >>= strToText
-- ""
-- >>> testJSaddle $ valToJSON 0 "1" >>= strToText
-- "1"
-- >>> testJSaddle $ obj >>= valToJSON 0 >>= strToText
-- {}
valToJSON :: ToJSVal value => value -> JSM JSString
#ifdef ghcjs_HOST_OS
valToJSON value = jsrefToJSON <$> toJSVal value
foreign import javascript unsafe "$r = JSON.stringify($1);" jsrefToJSON :: JSVal -> JSString
#else
valToJSON value =
    withToJSVal value $ \rval -> do
        ValueToJSONResult result <- sendCommand (ValueToJSON rval)
        wrapJSString result
#endif

-- | Given a JavaScript value get its object value.
--   May throw JSException.
--
-- >>> testJSaddle $ (valToObject JSNull >>= valToText) `catch` \ (JSException e) -> valToText e
-- TypeError:...null...is not an object
-- >>> testJSaddle $ (valToObject () >>= valToText) `catch` \ (JSException e) -> valToText e
-- TypeError:...undefined...is not an object
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
    makeObject = valToObject

-- | Convert to a JavaScript value (just an alias for 'toJSVal')
val :: ToJSVal value
    => value          -- ^ value to convert to a JavaScript value
    -> JSM JSVal
val = toJSVal

-- | If we already have a JSVal we are fine
instance ToJSVal JSVal where
    toJSVal = return

-- | A single JSVal can be used as the argument list
instance MakeArgs JSVal where
    makeArgs arg = return [arg]

-- | JSVal can be made by evaluating a function in 'JSM' as long
--   as it returns something we can make into a JSVal.
instance ToJSVal v => ToJSVal (JSM v) where
    toJSVal v = v >>= toJSVal

----------- null ---------------
-- | A @null@ JavaScript value
valNull :: JSVal
#ifdef ghcjs_HOST_OS
valNull = jsNull
#else
valNull = JSVal 0
#endif

-- | Makes a @null@ JavaScript value
instance ToJSVal JSNull where
    toJSVal = const (return valNull)

-- | Makes an argument list with just a single @null@ JavaScript value
instance MakeArgs JSNull where
    makeArgs _ = return [valNull]

-- | Makes a JSVal or @null@ JavaScript value
instance ToJSVal a => ToJSVal (Maybe a) where
    toJSVal Nothing = return valNull
    toJSVal (Just a) = toJSVal a

-- | Test a JavaScript value to see if it is @null@
valIsNull :: ToJSVal value => value -> JSM Bool
#ifdef ghcjs_HOST_OS
valIsNull value = isNull <$> toJSVal value
#else
valIsNull value = toJSVal value >>= \case
                        JSVal 0 -> return True
                        _       -> return False
#endif

----------- undefined ---------------
-- | An @undefined@ JavaScript value
valUndefined :: JSVal
#ifdef ghcjs_HOST_OS
valUndefined = jsUndefined
#else
valUndefined = JSVal 1
#endif

-- | Makes an @undefined@ JavaScript value
instance ToJSVal JSUndefined where
    toJSVal = const (return valUndefined)

--We can't allow this if JSUndefined is () as () is no args not "(null)".
--Use [()] instead.
--instance MakeArgs JSUndefined where
--    makeArgs _ = valMakeUndefined >>= (\ref -> return [ref])

-- | This allows us to pass no arguments easily (altenative would be to use @[]::[JSVal]@).
instance MakeArgs () where
    makeArgs _ = return []

-- | Test a JavaScript value to see if it is @undefined@
valIsUndefined :: ToJSVal value => value -> JSM Bool
#ifdef ghcjs_HOST_OS
valIsUndefined value = isUndefined <$> toJSVal value
#else
valIsUndefined value = toJSVal value >>= \case
                            JSVal ref -> return $ ref == 1
#endif

-- | Convert a JSVal to a Maybe JSVal (converting null and undefined to Nothing)
maybeNullOrUndefined :: ToJSVal value => value -> JSM (Maybe JSVal)
maybeNullOrUndefined value = do
    rval <- toJSVal value
    valIsNull rval >>= \case
        True -> return Nothing
        _    ->
            valIsUndefined rval >>= \case
                True -> return Nothing
                _    -> return (Just rval)

maybeNullOrUndefined' :: ToJSVal value => (JSVal -> JSM a) -> value -> JSM (Maybe a)
maybeNullOrUndefined' f value = do
    rval <- toJSVal value
    valIsNull rval >>= \case
        True -> return Nothing
        _    ->
            valIsUndefined rval >>= \case
                True -> return Nothing
                _    -> Just <$> f rval

----------- booleans ---------------
-- | A JavaScript boolean value
valBool :: Bool -> JSVal
#ifdef ghcjs_HOST_OS
valBool b = toJSBool b
#else
valBool b = JSVal $ if b then 3 else 2
#endif

-- | Make a JavaScript boolean value
instance ToJSVal Bool where
    toJSVal = return . valBool

-- | Makes an argument list with just a single JavaScript boolean value
instance MakeArgs Bool where
    makeArgs b = return [valBool b]

----------- numbers ---------------
-- | Make a JavaScript number
valMakeNumber :: Double -> JSM JSVal
#ifdef ghcjs_HOST_OS
valMakeNumber n = liftIO $ GHCJS.toJSVal n
#else
valMakeNumber n = do
    NumberToValueResult result <- sendCommand (NumberToValue n)
    wrapJSVal result
#endif

-- | Makes a JavaScript number
instance ToJSVal Double where
    toJSVal = valMakeNumber

instance ToJSVal Float where
    toJSVal = valMakeNumber . realToFrac

instance ToJSVal Word where
    toJSVal = valMakeNumber . fromIntegral

instance ToJSVal Word32 where
    toJSVal = valMakeNumber . fromIntegral

instance ToJSVal Word64 where
    toJSVal = valMakeNumber . fromIntegral

instance ToJSVal Int where
    toJSVal = valMakeNumber . fromIntegral

instance ToJSVal Int32 where
    toJSVal = valMakeNumber . fromIntegral

instance ToJSVal Int64 where
    toJSVal = valMakeNumber . fromIntegral

-- | Makes an argument list with just a single JavaScript number
instance MakeArgs Double where
    makeArgs n = valMakeNumber n >>= (\ref -> return [ref])

-- | Make a JavaScript string from `Text`
valMakeText :: Text -> JSM JSVal
#ifdef ghcjs_HOST_OS
valMakeText = return . pToJSVal . textToJSString
#else
valMakeText text = valString <$> textToStr text
#endif

-- | Make a JavaScript string from `JSString`
valString :: JSString -> JSVal
#ifdef ghcjs_HOST_OS
valString = pToJSVal
#else
valString (JSString ref) = JSVal ref
#endif

-- | Makes a JavaScript string
instance ToJSVal Text where
    toJSVal = valMakeText

-- | Makes an argument list with just a single JavaScript string
instance MakeArgs Text where
    makeArgs t = valMakeText t >>= (\ref -> return [ref])

-- | Makes a JavaScript string
instance ToJSVal String where
    toJSVal = valMakeText . T.pack

-- | Makes a JavaScript string
instance ToJSVal JSString where
    toJSVal = return . valString

-- | If we already have a JSString we are fine
instance ToJSString JSString where
    toJSString = return

instance ToJSString Text where
    toJSString = textToStr

instance ToJSString String where
    toJSString = textToStr . T.pack

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
foreign import javascript unsafe "$r = ($1 === undefined)?0:\
                                       ($1===null)?1:\
                                       (typeof $1===\"boolean\")?2:\
                                       (typeof $1===\"number\")?3:\
                                       (typeof $1===\"string\")?4:\
                                       (typeof $1===\"object\")?5:-1;" jsrefGetType :: JSVal -> Int
#else
deRefVal value = do
    v <- toJSVal value
    withJSVal v $ \rval ->
        sendCommand (DeRefVal rval) >>= \case
            DeRefValResult 0    _ -> return   ValNull
            DeRefValResult 1    _ -> return   ValUndefined
            DeRefValResult 2    _ -> return $ ValBool False
            DeRefValResult 3    _ -> return $ ValBool True
            DeRefValResult (-1) s -> return $ ValNumber (read (T.unpack s))
            DeRefValResult (-2) s -> return $ ValString s
            DeRefValResult ref  _ -> return $ ValObject (Object (JSVal ref))
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

strictEqual :: (ToJSVal a, ToJSVal b) => a -> b -> JSM Bool
strictEqual a b = do
    aval <- toJSVal a
    bval <- toJSVal b
#ifdef ghcjs_HOST_OS
    return $ jsvalueisstrictequal aval bval
#else
    withJSVal aval $ \aref ->
        withJSVal bval $ \bref -> do
            StrictEqualResult result <- sendCommand $ StrictEqual aref bref
            return result
#endif

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "\
    try {\
        $r = $1 instanceof $2\
    }\
    catch(e) {\
        $3[0] = e;\
    }"
  js_isInstanceOf :: JSVal -> Object -> MutableJSArray -> Bool
#endif

instanceOf :: (ToJSVal value, MakeObject constructor) => value -> constructor -> JSM Bool
instanceOf value constructor = do
    v <- toJSVal value
    c <- makeObject constructor
#ifdef ghcjs_HOST_OS
    rethrow $ return . js_isInstanceOf v c
#else
    withJSVal v $ \rval ->
        withObject c $ \c' -> do
            InstanceOfResult result <- sendCommand $ InstanceOf rval c'
            return result
#endif












