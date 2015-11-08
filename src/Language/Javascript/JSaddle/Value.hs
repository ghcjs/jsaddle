{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
#endif
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
    JSValueRef
  , MakeValueRef(..)

  -- * Haskell types for JavaScript values
  , JSNull(..)
  , JSUndefined(..)
  , JSBool(..)
  , JSNumber(..)
  , JSString(..)
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
  , valMakeUndefined
  , valMakeBool
  , valMakeNumber
  , valMakeString

  -- * Conver to and from JSValue
  , deRefVal
  , valMakeRef
) where

import Prelude hiding (catch)
import Language.Javascript.JSaddle.Types
       (JSValueRefRef, Object(..), JSStringRef, JSValueRef(..))
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import GHCJS.Types (JSVal(..))
import GHCJS.Foreign (toJSBool, isTruthy, jsNull, jsUndefined)
import GHCJS.Marshal (toJSVal)
import GHCJS.Marshal.Pure (pToJSVal)
import Data.JSString.Text (textToJSString)
#else
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
       (jsvaluecreatejsonstring, JSType(..), jsvaluegettype,
        jsvaluemakestring, jsvaluemakenumber, jsvaluemakeboolean,
        jsvaluemakeundefined, jsvaluemakenull, jsvaluetoobject,
        jsvaluetostringcopy, jsvaluetonumber, jsvaluetoboolean)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
       (jsstringgetcharactersptr, jsstringgetlength)
#endif
import Language.Javascript.JSaddle.Monad (JSM, catchval)
import Language.Javascript.JSaddle.Exception (rethrow)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO, MonadIO(..))
import qualified Data.Text.Foreign as T (fromPtr)
import Foreign (castPtr)
import Data.Text.Foreign (useAsPtr)
import Control.Applicative ((<$>))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Language.Javascript.JSaddle.Classes
       (MakeObject(..), MakeStringRef(..), MakeValueRef(..),
        MakeArgRefs(..))
import Language.Javascript.JSaddle.String (strToText, textToStr)
import Language.Javascript.JSaddle.Arguments ()
import Data.Word (Word)

data JSNull      = JSNull -- ^ Type that represents a value that can only be null.
                          --   Haskell of course has no null so we are adding this type.
type JSUndefined = ()     -- ^ A type that can only be undefined in JavaScript.  Using ()
                          --   because functions in JavaScript that have no return, impicitly
                          --   return undefined.
type JSBool      = Bool   -- ^ JavaScript boolean values map the 'Bool' haskell type.
type JSNumber    = Double -- ^ A number in JavaScript maps nicely to 'Double'.
type JSString    = Text   -- ^ JavaScript strings can be represented with the Haskell 'Text' type.

-- | An algebraic data type that can represent a JavaScript value.  Any JavaScriptCore
--   'JSValueRef' can be converted into this type.
data JSValue = ValNull                   -- ^ null
             | ValUndefined              -- ^ undefined
             | ValBool      JSBool       -- ^ true or false
             | ValNumber    JSNumber     -- ^ a number
             | ValString    JSString     -- ^ a string
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
valToBool :: MakeValueRef val => val -> JSM JSBool
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valToBool val = isTruthy <$> makeValueRef val
#else
valToBool val = do
    gctxt <- ask
    rval <- makeValueRef val
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
valToNumber :: MakeValueRef val => val -> JSM JSNumber
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
valToNumber val = jsrefToNumber <$> makeValueRef val
{-# INLINE valToNumber #-}
foreign import javascript unsafe "$r = Number($1);" jsrefToNumber :: JSVal -> Double
#elif defined(USE_WEBKIT)
valToNumber val = do
    gctxt <- ask
    rval <- makeValueRef val
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
valToStr :: MakeValueRef val => val -> JSM JSStringRef
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
valToStr val = jsrefToString <$> makeValueRef val
{-# INLINE valToStr #-}
foreign import javascript unsafe "$r = $1.toString();" jsrefToString :: JSVal -> JSStringRef
#elif defined(USE_WEBKIT)
valToStr val = do
    gctxt <- ask
    rval <- makeValueRef val
    rethrow $ liftIO . jsvaluetostringcopy gctxt rval
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
valToText :: MakeValueRef val => val -> JSM Text
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
valToJSON :: MakeValueRef val => Word -> val -> JSM JSStringRef
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
valToJSON indent val = jsrefToJSON <$> makeValueRef val
{-# INLINE valToJSON #-}
foreign import javascript unsafe "$r = JSON.stringify($1);" jsrefToJSON :: JSVal -> JSStringRef
#elif defined(USE_WEBKIT)
valToJSON indent val = do
    gctxt <- ask
    rval <- makeValueRef val
    rethrow $ liftIO . jsvaluecreatejsonstring gctxt rval (fromIntegral indent)
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
valToObject :: MakeValueRef val => val -> JSM Object
valToObject val = Object <$>
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
    makeValueRef val
#else
 do gctxt <- ask
    rval <- makeValueRef val
    rethrow $ liftIO . jsvaluetoobject gctxt rval
#endif
{-# INLINE valToObject #-}

instance MakeObject JSValueRef where
    makeObject = valToObject
    {-# INLINE makeObject #-}

-- | Convert to a JavaScript value (just an alias for 'makeValueRef')
val :: MakeValueRef value
    => value          -- ^ value to convert to a JavaScript value
    -> JSM JSValueRef
val = makeValueRef
{-# INLINE val #-}

-- | If we already have a JSValueRef we are fine
instance MakeValueRef JSValueRef where
    makeValueRef = return
    {-# INLINE makeValueRef #-}

-- | A single JSValueRef can be used as the argument list
instance MakeArgRefs JSValueRef where
    makeArgRefs arg = return [arg]
    {-# INLINE makeArgRefs #-}

-- | JSValueRef can be made by evaluating a function in 'JSM' as long
--   as it returns something we can make into a JSValueRef.
instance MakeValueRef v => MakeValueRef (JSM v) where
    makeValueRef v = v >>= makeValueRef
    {-# INLINE makeValueRef #-}

----------- null ---------------
-- | Make a @null@ JavaScript value
valMakeNull :: JSM JSValueRef
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valMakeNull = return jsNull
#else
valMakeNull = ask >>= (liftIO . jsvaluemakenull)
#endif
{-# INLINE valMakeNull #-}

-- | Makes a @null@ JavaScript value
instance MakeValueRef JSNull where
    makeValueRef = const valMakeNull
    {-# INLINE makeValueRef #-}

-- | Makes an argument list with just a single @null@ JavaScript value
instance MakeArgRefs JSNull where
    makeArgRefs _ = valMakeNull >>= (\ref -> return [ref])
    {-# INLINE makeArgRefs #-}

----------- undefined ---------------
-- | Make an @undefined@ JavaScript value
valMakeUndefined :: JSM JSValueRef
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valMakeUndefined = return jsUndefined
#else
valMakeUndefined = ask >>= (liftIO . jsvaluemakeundefined)
#endif
{-# INLINE valMakeUndefined #-}

-- | Makes an @undefined@ JavaScript value
instance MakeValueRef JSUndefined where
    makeValueRef = const valMakeUndefined
    {-# INLINE makeValueRef #-}

--We can't allow this if JSUndefined is () as () is no args not "(null)".
--Use [()] instead.
--instance MakeArgRefs JSUndefined where
--    makeArgRefs _ = valMakeUndefined >>= (\ref -> return [ref])

-- | This allows us to pass no arguments easily (altenative would be to use @[]::[JSValueRef]@).
instance MakeArgRefs () where
    makeArgRefs _ = return []
    {-# INLINE makeArgRefs #-}

----------- booleans ---------------
-- | Make a JavaScript boolean value
valMakeBool :: JSBool -> JSM JSValueRef
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valMakeBool b = return  $ toJSBool b
#else
valMakeBool b = do
    gctxt <- ask
    liftIO $ jsvaluemakeboolean gctxt b
#endif
{-# INLINE valMakeBool #-}

-- | Make a JavaScript boolean value
instance MakeValueRef Bool where
    makeValueRef = valMakeBool
    {-# INLINE makeValueRef #-}

-- | Makes an argument list with just a single JavaScript boolean value
instance MakeArgRefs Bool where
    makeArgRefs b = valMakeBool b >>= (\ref -> return [ref])
    {-# INLINE makeArgRefs #-}

----------- numbers ---------------
-- | Make a JavaScript number
valMakeNumber :: JSNumber -> JSM JSValueRef
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valMakeNumber n = liftIO $ toJSVal n
#else
valMakeNumber n = do
    gctxt <- ask
    liftIO $ jsvaluemakenumber gctxt n
#endif
{-# INLINE valMakeNumber #-}

-- | Makes a JavaScript number
instance MakeValueRef Double where
    makeValueRef = valMakeNumber
    {-# INLINE makeValueRef #-}

-- | Makes an argument list with just a single JavaScript number
instance MakeArgRefs Double where
    makeArgRefs n = valMakeNumber n >>= (\ref -> return [ref])
    {-# INLINE makeArgRefs #-}

----------- numbers ---------------
-- | Make a JavaScript string
valMakeString :: Text -> JSM JSValueRef
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
valMakeString = return . pToJSVal . textToJSString
#else
valMakeString text = do
    gctxt <- ask
    liftIO $ jsvaluemakestring gctxt (textToStr text)
#endif
{-# INLINE valMakeString #-}

-- | Makes a JavaScript string
instance MakeValueRef Text where
    makeValueRef = valMakeString
    {-# INLINE makeValueRef #-}

-- | Makes an argument list with just a single JavaScript string
instance MakeArgRefs Text where
    makeArgRefs t = valMakeString t >>= (\ref -> return [ref])
    {-# INLINE makeArgRefs #-}

-- | Makes a JavaScript string
instance MakeValueRef String where
    makeValueRef = valMakeString . T.pack
    {-# INLINE makeValueRef #-}

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
deRefVal :: MakeValueRef val => val -> JSM JSValue
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
deRefVal val = do
    gctxt <- ask
    valref <- makeValueRef val
    case (jsrefGetType valref :: Int) of
        0 -> return ValUndefined
        1 -> return ValNull
        2 -> ValBool   <$> valToBool valref
        3 -> ValNumber <$> valToNumber valref
        4 -> ValString <$> (valToStr valref >>= strToText)
        5 -> ValObject <$> valToObject valref
foreign import javascript unsafe "$r = ($1 === undefined)?0:\
                                       ($1===null)?1:\
                                       (typeof $1===\"boolean\")?2:\
                                       (typeof $1===\"number\")?3:\
                                       (typeof $1===\"string\")?4:\
                                       (typeof $1===\"object\")?5:-1;" jsrefGetType :: JSValueRef -> Int
#elif defined(USE_WEBKIT)
deRefVal val = do
    gctxt <- ask
    valref <- makeValueRef val
    t <- liftIO $ jsvaluegettype gctxt valref
    case t of
        Kjstypenull      -> return ValNull
        Kjstypeundefined -> return ValUndefined
        Kjstypeboolean   -> ValBool   <$> valToBool valref
        Kjstypenumber    -> ValNumber <$> valToNumber valref
        Kjstypestring    -> ValString <$> (valToStr valref >>= strToText)
        Kjstypeobject    -> ValObject <$> valToObject valref
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
valMakeRef :: JSValue -> JSM JSValueRef
valMakeRef val =
    case val of
        ValNull              -> valMakeNull
        ValUndefined         -> valMakeUndefined
        ValBool b            -> valMakeBool b
        ValNumber n          -> valMakeNumber n
        ValString s          -> valMakeString s
        ValObject (Object o) -> return o

-- | Makes a JavaScript value from a 'JSValue' ADT.
instance MakeValueRef JSValue where
    makeValueRef = valMakeRef
    {-# INLINE makeValueRef #-}

-- | Makes an argument list with just a single JavaScript value from a 'JSValue' ADT.
instance MakeArgRefs JSValue where
    makeArgRefs v = valMakeRef v >>= (\ref -> return [ref])
    {-# INLINE makeArgRefs #-}

--instance MakeObjectRef JSNull where
--    makeObjectRef _ = Object <$> valMakeNull
--    {-# INLINE makeObjectRef #-}

