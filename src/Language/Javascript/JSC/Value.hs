{-# LANGUAGE ForeignFunctionInterface
           , TypeSynonymInstances
           , FlexibleInstances
           , DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC.Value
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSC.Value (
    JSNull(..)
  , JSUndefined
  , JSBool
  , JSNumber
  , JSString
  , JSValue(..)

  , JSStringRef
  , JSObjectRef
  , JSValueRef

  , valToBool
  , valToNumber
  , valToStr
  , valToObject

  , strToText
  , textToStr

  , valToText

  , rethrow

  , MakeValueRef(..)
  , MakeStringRef(..)
  , MakeArgRefs(..)

  , val
  , valMakeNull
  , valMakeUndefined
  , valMakeBool
  , valMakeNumber
  , valMakeString

  , deRefVal
  , valMakeRef
) where

import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSValueRefRef, JSObjectRef, JSStringRef, JSValueRef)
import Language.Javascript.JSC.Monad (JSC, catch)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO, MonadIO(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
       (JSType(..), jsvaluegettype, jsvaluemakestring, jsvaluemakenumber,
        jsvaluemakeboolean, jsvaluemakeundefined, jsvaluemakenull,
        jsvaluetoobject, jsvaluetostringcopy, jsvaluetonumber,
        jsvaluetoboolean)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
       (jsstringcreatewithcharacters, jsstringgetcharactersptr,
        jsstringgetlength)
import qualified Data.Text.Foreign as T (fromPtr)
import Foreign (castPtr)
import Data.Text.Foreign (useAsPtr)
import Control.Applicative ((<$>))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Control.Exception as E (throwIO, Exception)
import Data.Typeable (Typeable)

data JSNull      = JSNull
type JSUndefined = ()
type JSBool      = Bool
type JSNumber    = Double
type JSString    = Text

data JSValue = ValNull
             | ValUndefined
             | ValBool      JSBool
             | ValNumber    JSNumber
             | ValString    JSString
             | ValObject    JSObjectRef deriving(Show, Eq)

valToBool :: JSValueRef -> JSC JSBool
valToBool jsvar = do
    gctxt <- ask
    liftIO $ jsvaluetoboolean gctxt jsvar

valToNumber :: JSValueRef -> JSC JSNumber
valToNumber jsvar = do
    gctxt <- ask
    rethrow $ liftIO . jsvaluetonumber gctxt jsvar

valToStr :: JSValueRef -> JSC JSStringRef
valToStr jsvar = do
    gctxt <- ask
    rethrow $ liftIO . jsvaluetostringcopy gctxt jsvar

valToObject :: JSValueRef -> JSC JSObjectRef
valToObject jsvar = do
    gctxt <- ask
    rethrow $ liftIO . jsvaluetoobject gctxt jsvar

strToText :: MonadIO m => JSStringRef -> m Text
strToText jsstring = liftIO $ do
    l <- jsstringgetlength jsstring
    p <- jsstringgetcharactersptr jsstring
    T.fromPtr (castPtr p) (fromIntegral l)

textToStr :: MonadIO m => Text -> m JSStringRef
textToStr text = do
    liftIO $ useAsPtr text $ \p l -> do
        jsstringcreatewithcharacters (castPtr p) (fromIntegral l)

valToText :: JSValueRef -> JSC Text
valToText jsvar = valToStr jsvar >>= strToText

data JSException = JSException String JSValueRef deriving (Show, Typeable)

instance E.Exception JSException

rethrow :: (JSValueRefRef -> JSC a) -> JSC a
rethrow f = f `catch` \e -> do
    s <- deRefVal e
    liftIO . E.throwIO $ JSException (show s) e

class MakeValueRef a where
    makeValueRef :: a -> JSC JSValueRef

class MakeStringRef a where
    makeStringRef :: MonadIO m => a -> m JSStringRef

---- Arguments ----
class MakeArgRefs this where
    makeArgRefs :: this -> JSC [JSValueRef]

instance MakeArgRefs arg => MakeArgRefs (JSC arg) where
    makeArgRefs arg = arg >>= makeArgRefs

instance MakeValueRef arg => MakeArgRefs [arg] where
    makeArgRefs = mapM makeValueRef

instance MakeArgRefs () where
    makeArgRefs _ = return []

instance (MakeValueRef arg1, MakeValueRef arg2) => MakeArgRefs (arg1, arg2) where
    makeArgRefs (arg1, arg2) = do
        rarg1 <- makeValueRef arg1
        rarg2 <- makeValueRef arg2
        return [rarg1, rarg2]

instance (MakeValueRef arg1, MakeValueRef arg2, MakeValueRef arg3) => MakeArgRefs (arg1, arg2, arg3) where
    makeArgRefs (arg1, arg2, arg3) = do
        rarg1 <- makeValueRef arg1
        rarg2 <- makeValueRef arg2
        rarg3 <- makeValueRef arg3
        return [rarg1, rarg2, rarg3]

instance (MakeValueRef arg1, MakeValueRef arg2, MakeValueRef arg3, MakeValueRef arg4) => MakeArgRefs (arg1, arg2, arg3, arg4) where
    makeArgRefs (arg1, arg2, arg3, arg4) = do
        rarg1 <- makeValueRef arg1
        rarg2 <- makeValueRef arg2
        rarg3 <- makeValueRef arg3
        rarg4 <- makeValueRef arg4
        return [rarg1, rarg2, rarg3, rarg4]

instance (MakeValueRef arg1, MakeValueRef arg2, MakeValueRef arg3, MakeValueRef arg4, MakeValueRef arg5) => MakeArgRefs (arg1, arg2, arg3, arg4, arg5) where
    makeArgRefs (arg1, arg2, arg3, arg4, arg5) = do
        rarg1 <- makeValueRef arg1
        rarg2 <- makeValueRef arg2
        rarg3 <- makeValueRef arg3
        rarg4 <- makeValueRef arg4
        rarg5 <- makeValueRef arg5
        return [rarg1, rarg2, rarg3, rarg4, rarg5]

val :: MakeValueRef v => v -> JSC JSValueRef
val = makeValueRef

instance MakeValueRef JSValueRef where
    makeValueRef = return

instance MakeArgRefs JSValueRef where
    makeArgRefs arg = return [arg]

instance MakeValueRef v => MakeValueRef (JSC v) where
    makeValueRef v = v >>= makeValueRef

instance MakeStringRef JSStringRef where
    makeStringRef = return

instance MakeStringRef Text where
    makeStringRef = textToStr

instance MakeStringRef String where
    makeStringRef = textToStr . T.pack

valMakeNull :: JSC JSValueRef
valMakeNull = ask >>= (liftIO . jsvaluemakenull)

instance MakeValueRef JSNull where
    makeValueRef = const valMakeNull

instance MakeArgRefs JSNull where
    makeArgRefs _ = valMakeNull >>= (\ref -> return [ref])

valMakeUndefined :: JSC JSValueRef
valMakeUndefined = ask >>= (liftIO . jsvaluemakeundefined)

instance MakeValueRef JSUndefined where
    makeValueRef = const valMakeUndefined

--We can't allow this if JSUndefined is () as () is now args not "(null)"
--instance MakeArgRefs JSUndefined where
--    makeArgRefs _ = valMakeUndefined >>= (\ref -> return [ref])

valMakeBool :: JSBool -> JSC JSValueRef
valMakeBool b = do
    gctxt <- ask
    liftIO $ jsvaluemakeboolean gctxt b

instance MakeValueRef Bool where
    makeValueRef = valMakeBool

instance MakeArgRefs Bool where
    makeArgRefs b = valMakeBool b >>= (\ref -> return [ref])

valMakeNumber :: JSNumber -> JSC JSValueRef
valMakeNumber n = do
    gctxt <- ask
    liftIO $ jsvaluemakenumber gctxt n

instance MakeValueRef Double where
    makeValueRef = valMakeNumber

instance MakeArgRefs Double where
    makeArgRefs n = valMakeNumber n >>= (\ref -> return [ref])

valMakeString :: Text -> JSC JSValueRef
valMakeString text = do
    gctxt <- ask
    s <- textToStr text
    liftIO $ jsvaluemakestring gctxt s

instance MakeValueRef Text where
    makeValueRef = valMakeString

instance MakeArgRefs Text where
    makeArgRefs t = valMakeString t >>= (\ref -> return [ref])

instance MakeValueRef String where
    makeValueRef = valMakeString . T.pack

deRefVal :: MakeValueRef val => val -> JSC JSValue
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

valMakeRef :: JSValue -> JSC JSValueRef
valMakeRef val = do
    case val of
        ValNull      -> valMakeNull
        ValUndefined -> valMakeUndefined
        ValBool b    -> valMakeBool b
        ValNumber n  -> valMakeNumber n
        ValString s  -> valMakeString s
        ValObject o  -> return o

instance MakeValueRef JSValue where
    makeValueRef = valMakeRef

instance MakeArgRefs JSValue where
    makeArgRefs v = valMakeRef v >>= (\ref -> return [ref])

