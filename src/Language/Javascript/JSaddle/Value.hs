{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LambdaCase #-}
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
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

  -- * Converting JavaScript values
  , valToBool
  , valToNumber
  , valToStr
  , valToObject
  , valToText
  , valToJSON

  -- * Make JavaScript values from Haskell ones
  , val
  , valMakeNull
  , valIsNull
  , valMakeUndefined
  , valIsUndefined
  , maybeNullOrUndefined
  , maybeNullOrUndefined'
  , valMakeBool
  , valMakeNumber
  , valMakeString

  -- * Convert to and from JSValue
  , deRefVal
  , valMakeRef
  , strictEqual
  , instanceOf
) where

import Prelude hiding (catch)
import Language.Javascript.JSaddle.Types
       (Object(..), JSString, JSVal)
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import Language.Javascript.JSaddle.Types
       (MutableJSArray)
import GHCJS.Types (JSVal(..), isNull, isUndefined)
import GHCJS.Foreign (toJSBool, isTruthy, jsNull, jsUndefined)
import qualified GHCJS.Marshal as GHCJS (toJSVal)
import GHCJS.Marshal.Pure (pToJSVal)
import Data.JSString.Text (textToJSString)
#else
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
       (jsvalueisinstanceofconstructor, jsvaluecreatejsonstring,
        JSType(..), jsvaluegettype, jsvaluemakestring, jsvaluemakenumber,
        jsvaluemakeboolean, jsvaluemakeundefined, jsvaluemakenull,
        jsvaluetoobject, jsvaluetostringcopy, jsvaluetonumber,
        jsvaluetoboolean, jsvalueisnull, jsvalueisundefined,
        jsvalueisstrictequal)
import Language.Javascript.JSaddle.Native
       (makeNewJSVal, wrapJSString, withJSVal, withObject, withJSString,
        withToJSVal)
#endif
import Language.Javascript.JSaddle.Monad (JSM)
import Language.Javascript.JSaddle.Exception (rethrow)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Language.Javascript.JSaddle.Classes
       (MakeObject(..), ToJSString(..), ToJSVal(..))
import Language.Javascript.JSaddle.String (strToText, textToStr)
import Language.Javascript.JSaddle.Arguments (MakeArgs(..))
import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)

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
--             deriving(Show, Eq)

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
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valToBool value = isTruthy <$> toJSVal value
#else
valToBool value = do
    gctxt <- ask
    withToJSVal value $ \rval ->
        liftIO $ jsvaluetoboolean gctxt rval
#endif
{-# INLINE valToBool #-}

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
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
valToNumber value = jsrefToNumber <$> toJSVal value
{-# INLINE valToNumber #-}
foreign import javascript unsafe "$r = Number($1);" jsrefToNumber :: JSVal -> Double
#elif defined(USE_WEBKIT)
valToNumber value = do
    gctxt <- ask
    withToJSVal value $ \rval ->
        rethrow $ liftIO . jsvaluetonumber gctxt rval
{-# INLINE valToNumber #-}
#else
valToNumber = undefined
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
--
-- >>> testJSaddle $ valToStr "1" >>= strToText
-- 1
valToStr :: ToJSVal value => value -> JSM JSString
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
valToStr value = jsrefToString <$> toJSVal value
{-# INLINE valToStr #-}
foreign import javascript unsafe "$r = $1.toString();" jsrefToString :: JSVal -> JSString
#elif defined(USE_WEBKIT)
valToStr value = do
    gctxt <- ask
    withToJSVal value $ \rval ->
        (rethrow $ liftIO . jsvaluetostringcopy gctxt rval) >>= wrapJSString
{-# INLINE valToStr #-}
#else
valToStr = undefined
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
{-# INLINE valToText #-}

-- | Given a JavaScript value get a JSON string value.
--   May throw JSException.
--
-- >>> testJSaddle $ valToJSON 0 JSNull >>= strToText
-- null
-- >>> testJSaddle $ valToJSON 0 () >>= strToText
--
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
valToJSON :: ToJSVal value => Word -> value -> JSM JSString
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
valToJSON indent value = jsrefToJSON <$> toJSVal value
{-# INLINE valToJSON #-}
foreign import javascript unsafe "$r = JSON.stringify($1);" jsrefToJSON :: JSVal -> JSString
#elif defined(USE_WEBKIT)
valToJSON indent value = do
    gctxt <- ask
    withToJSVal value $ \rval ->
        (rethrow $ liftIO . jsvaluecreatejsonstring gctxt rval (fromIntegral indent)) >>= wrapJSString
{-# INLINE valToJSON #-}
#else
valToJSON = undefined
#endif

-- | Given a JavaScript value get its object value.
--   May throw JSException.
--
-- >>> testJSaddle $ (valToObject JSNull >>= valToText) `catch` \ (JSException e) -> valToText e
-- TypeError: 'null' is not an object
-- >>> testJSaddle $ (valToObject () >>= valToText) `catch` \ (JSException e) -> valToText e
-- TypeError: 'undefined' is not an object
-- >>> testJSaddle $ valToObject True
-- true
-- >>> testJSaddle $ valToObject False
-- false
-- >>> testJSaddle $ valToObject (1.0 :: Double)
-- 1
-- >>> testJSaddle $ valToObject (0.0 :: Double)
-- 0
-- >>> testJSaddle $ valToObject ""
--
-- >>> testJSaddle $ valToObject "1"
-- 1
valToObject :: ToJSVal value => value -> JSM Object
valToObject value = Object <$>
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
    toJSVal value
#else
 do gctxt <- ask
    withToJSVal value $ \rval ->
        (rethrow $ liftIO . jsvaluetoobject gctxt rval) >>= makeNewJSVal
#endif
{-# INLINE valToObject #-}

instance MakeObject JSVal where
    makeObject = valToObject
    {-# INLINE makeObject #-}

-- | Convert to a JavaScript value (just an alias for 'toJSVal')
val :: ToJSVal value
    => value          -- ^ value to convert to a JavaScript value
    -> JSM JSVal
val = toJSVal
{-# INLINE val #-}

-- | If we already have a JSVal we are fine
instance ToJSVal JSVal where
    toJSVal = return
    {-# INLINE toJSVal #-}

-- | A single JSVal can be used as the argument list
instance MakeArgs JSVal where
    makeArgs arg = return [arg]
    {-# INLINE makeArgs #-}

-- | JSVal can be made by evaluating a function in 'JSM' as long
--   as it returns something we can make into a JSVal.
instance ToJSVal v => ToJSVal (JSM v) where
    toJSVal v = v >>= toJSVal
    {-# INLINE toJSVal #-}

----------- null ---------------
-- | Make a @null@ JavaScript value
valMakeNull :: JSM JSVal
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valMakeNull = return jsNull
#else
valMakeNull = ask >>= (liftIO . jsvaluemakenull) >>= makeNewJSVal
#endif
{-# INLINE valMakeNull #-}

-- | Makes a @null@ JavaScript value
instance ToJSVal JSNull where
    toJSVal = const valMakeNull
    {-# INLINE toJSVal #-}

-- | Makes an argument list with just a single @null@ JavaScript value
instance MakeArgs JSNull where
    makeArgs _ = valMakeNull >>= (\ref -> return [ref])
    {-# INLINE makeArgs #-}

-- | Makes a JSVal or @null@ JavaScript value
instance ToJSVal a => ToJSVal (Maybe a) where
    toJSVal Nothing = valMakeNull
    toJSVal (Just a) = toJSVal a
    {-# INLINE toJSVal #-}

-- | Test a JavaScript value to see if it is @null@
valIsNull :: ToJSVal value => value -> JSM Bool
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valIsNull value = isNull <$> toJSVal value
#else
valIsNull value = do
    gctxt <- ask
    withToJSVal value $ \rval ->
        liftIO $ jsvalueisnull gctxt rval
#endif
{-# INLINE valIsNull #-}

----------- undefined ---------------
-- | Make an @undefined@ JavaScript value
valMakeUndefined :: JSM JSVal
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valMakeUndefined = return jsUndefined
#else
valMakeUndefined = ask >>= (liftIO . jsvaluemakeundefined) >>= makeNewJSVal
#endif
{-# INLINE valMakeUndefined #-}

-- | Makes an @undefined@ JavaScript value
instance ToJSVal JSUndefined where
    toJSVal = const valMakeUndefined
    {-# INLINE toJSVal #-}

--We can't allow this if JSUndefined is () as () is no args not "(null)".
--Use [()] instead.
--instance MakeArgs JSUndefined where
--    makeArgs _ = valMakeUndefined >>= (\ref -> return [ref])

-- | This allows us to pass no arguments easily (altenative would be to use @[]::[JSVal]@).
instance MakeArgs () where
    makeArgs _ = return []
    {-# INLINE makeArgs #-}

-- | Test a JavaScript value to see if it is @undefined@
valIsUndefined :: ToJSVal value => value -> JSM Bool
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valIsUndefined value = isUndefined <$> toJSVal value
#else
valIsUndefined value = do
    gctxt <- ask
    withToJSVal value $ \rval ->
        liftIO $ jsvalueisundefined gctxt rval
#endif
{-# INLINE valIsUndefined #-}

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
{-# INLINE maybeNullOrUndefined #-}

maybeNullOrUndefined' :: ToJSVal value => (JSVal -> JSM a) -> value -> JSM (Maybe a)
maybeNullOrUndefined' f value = do
    rval <- toJSVal value
    valIsNull rval >>= \case
        True -> return Nothing
        _    ->
            valIsUndefined rval >>= \case
                True -> return Nothing
                _    -> Just <$> f rval
{-# INLINE maybeNullOrUndefined' #-}

----------- booleans ---------------
-- | Make a JavaScript boolean value
valMakeBool :: Bool -> JSM JSVal
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valMakeBool b = return  $ toJSBool b
#else
valMakeBool b = do
    gctxt <- ask
    (liftIO $ jsvaluemakeboolean gctxt b) >>= makeNewJSVal
#endif
{-# INLINE valMakeBool #-}

-- | Make a JavaScript boolean value
instance ToJSVal Bool where
    toJSVal = valMakeBool
    {-# INLINE toJSVal #-}

-- | Makes an argument list with just a single JavaScript boolean value
instance MakeArgs Bool where
    makeArgs b = valMakeBool b >>= (\ref -> return [ref])
    {-# INLINE makeArgs #-}

----------- numbers ---------------
-- | Make a JavaScript number
valMakeNumber :: Double -> JSM JSVal
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valMakeNumber n = liftIO $ GHCJS.toJSVal n
#else
valMakeNumber n = do
    gctxt <- ask
    (liftIO $ jsvaluemakenumber gctxt n) >>= makeNewJSVal
#endif
{-# INLINE valMakeNumber #-}

-- | Makes a JavaScript number
instance ToJSVal Double where
    toJSVal = valMakeNumber
    {-# INLINE toJSVal #-}

instance ToJSVal Float where
    toJSVal = valMakeNumber . realToFrac
    {-# INLINE toJSVal #-}

instance ToJSVal Word where
    toJSVal = valMakeNumber . fromIntegral
    {-# INLINE toJSVal #-}

instance ToJSVal Word32 where
    toJSVal = valMakeNumber . fromIntegral
    {-# INLINE toJSVal #-}

instance ToJSVal Word64 where
    toJSVal = valMakeNumber . fromIntegral
    {-# INLINE toJSVal #-}

instance ToJSVal Int where
    toJSVal = valMakeNumber . fromIntegral
    {-# INLINE toJSVal #-}

instance ToJSVal Int32 where
    toJSVal = valMakeNumber . fromIntegral
    {-# INLINE toJSVal #-}

instance ToJSVal Int64 where
    toJSVal = valMakeNumber . fromIntegral
    {-# INLINE toJSVal #-}

-- | Makes an argument list with just a single JavaScript number
instance MakeArgs Double where
    makeArgs n = valMakeNumber n >>= (\ref -> return [ref])
    {-# INLINE makeArgs #-}

-- | Make a JavaScript string from `Text`
valMakeText :: Text -> JSM JSVal
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valMakeText = return . pToJSVal . textToJSString
#else
valMakeText text = do
    gctxt <- ask
    withJSString (textToStr text) $ \s ->
        (liftIO $ jsvaluemakestring gctxt s) >>= makeNewJSVal
#endif
{-# INLINE valMakeText #-}

-- | Make a JavaScript string from `JSString`
valMakeString :: JSString -> JSM JSVal
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valMakeString = return . pToJSVal
#else
valMakeString str = do
    gctxt <- ask
    withJSString str $ \s ->
        (liftIO $ jsvaluemakestring gctxt s) >>= makeNewJSVal
#endif
{-# INLINE valMakeString #-}

-- | Makes a JavaScript string
instance ToJSVal Text where
    toJSVal = valMakeText
    {-# INLINE toJSVal #-}

-- | Makes an argument list with just a single JavaScript string
instance MakeArgs Text where
    makeArgs t = valMakeText t >>= (\ref -> return [ref])
    {-# INLINE makeArgs #-}

-- | Makes a JavaScript string
instance ToJSVal String where
    toJSVal = valMakeText . T.pack
    {-# INLINE toJSVal #-}

-- | Makes a JavaScript string
instance ToJSVal JSString where
    toJSVal = valMakeString
    {-# INLINE toJSVal #-}

-- | If we already have a JSString we are fine
instance ToJSString JSString where
    toJSString = id
    {-# INLINE toJSString #-}

instance ToJSString Text where
    toJSString = textToStr
    {-# INLINE toJSString #-}

instance ToJSString String where
    toJSString = textToStr . T.pack
    {-# INLINE toJSString #-}

-- | Derefernce a value reference.
--
-- >>> testJSaddle $ show <$> deRefVal JSNull
-- ValNull
-- >>> testJSaddle $ show <$> deRefVal ()
-- ValUndefined
-- >>> testJSaddle $ show <$> deRefVal True
-- ValBool True
-- >>> testJSaddle $ show <$> deRefVal False
-- ValBool False
-- >>> testJSaddle $ show <$> deRefVal (1.0 :: Double)
-- ValNumber 1.0
-- >>> testJSaddle $ show <$> deRefVal (0.0 :: Double)
-- ValNumber 0.0
-- >>> testJSaddle $ show <$> deRefVal ""
-- ValString ""
-- >>> testJSaddle $ show <$> deRefVal "1"
-- ValString "1"
-- >>> testJSaddle $ show <$> valToObject True >>= deRefVal
-- ValObject 0x...
deRefVal :: ToJSVal value => value -> JSM JSValue
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
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
#elif defined(USE_WEBKIT)
deRefVal value = do
    gctxt <- ask
    v <- toJSVal value
    withJSVal v $ \rval ->
        (liftIO $ jsvaluegettype gctxt rval) >>= \case
            Kjstypenull      -> return ValNull
            Kjstypeundefined -> return ValUndefined
            Kjstypeboolean   -> ValBool   <$> valToBool v
            Kjstypenumber    -> ValNumber <$> valToNumber v
            Kjstypestring    -> ValString <$> valToText v
            Kjstypeobject    -> ValObject <$> valToObject v
#else
deRefVal = undefined
#endif

-- | Make a JavaScript value out of a 'JSValue' ADT.
--
-- >>> testJSaddle $ valMakeRef ValNull
-- "null"
-- >>> testJSaddle $ valMakeRef ValUndefined
-- "undefined"
-- >>> testJSaddle $ valMakeRef (ValBool True)
-- "true"
-- >>> testJSaddle $ valMakeRef (ValNumber 1)
-- "1"
-- >>> testJSaddle $ valMakeRef (ValString $ pack "Hello")
-- "Hello"
valMakeRef :: JSValue -> JSM JSVal
valMakeRef value =
    case value of
        ValNull              -> valMakeNull
        ValUndefined         -> valMakeUndefined
        ValBool b            -> valMakeBool b
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
    {-# INLINE makeArgs #-}

--instance MakeObjectRef JSNull where
--    makeObjectRef _ = Object <$> valMakeNull
--    {-# INLINE makeObjectRef #-}

#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
foreign import javascript unsafe
  "$1===$2" jsvalueisstrictequal :: JSVal -> JSVal -> Bool
#endif

strictEqual :: (ToJSVal a, ToJSVal b) => a -> b -> JSM Bool
strictEqual a b = do
    aval <- toJSVal a
    bval <- toJSVal b
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
    return $ jsvalueisstrictequal aval bval
#else
    gctxt <- ask
    withJSVal aval $ \aref ->
        withJSVal bval $ \bref ->
            liftIO $ jsvalueisstrictequal gctxt aref bref
#endif

#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
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
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
    rethrow $ return . js_isInstanceOf v c
#else
    gctxt <- ask
    withJSVal v $ \rval ->
        withObject c $ \c' ->
            rethrow $ liftIO . jsvalueisinstanceofconstructor gctxt rval c'
#endif












