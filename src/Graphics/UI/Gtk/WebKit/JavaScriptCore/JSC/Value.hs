{-# LANGUAGE ForeignFunctionInterface
           , TypeSynonymInstances
           , FlexibleInstances
           , DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Value
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Value (
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

  , MakeValueRef(..)
  , MakeStringRef(..)

  , valMakeNull
  , valMakeUndefined
  , valMakeBool
  , valMakeNumber
  , valMakeString

  , deRefVal
  , valMakeRef
) where

import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSObjectRef, JSStringRef, JSValueRef)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Monad (rethrow, JSC)
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
             | ValObject    JSObjectRef deriving(Show)

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

class MakeValueRef a where
    makeValueRef :: a -> JSC JSValueRef

instance MakeValueRef JSValueRef where
    makeValueRef = return

class MakeStringRef a where
    makeStringRef :: MonadIO m => a -> m JSStringRef

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

valMakeUndefined :: JSC JSValueRef
valMakeUndefined = ask >>= (liftIO . jsvaluemakeundefined)

instance MakeValueRef JSUndefined where
    makeValueRef = const valMakeUndefined

valMakeBool :: JSBool -> JSC JSValueRef
valMakeBool b = do
    gctxt <- ask
    liftIO $ jsvaluemakeboolean gctxt b

instance MakeValueRef Bool where
    makeValueRef = valMakeBool

valMakeNumber :: JSNumber -> JSC JSValueRef
valMakeNumber n = do
    gctxt <- ask
    liftIO $ jsvaluemakenumber gctxt n

instance MakeValueRef Double where
    makeValueRef = valMakeNumber

valMakeString :: Text -> JSC JSValueRef
valMakeString text = do
    gctxt <- ask
    s <- textToStr text
    liftIO $ jsvaluemakestring gctxt s

instance MakeValueRef Text where
    makeValueRef = valMakeString

instance MakeValueRef String where
    makeValueRef = valMakeString . T.pack

deRefVal :: JSValueRef -> JSC JSValue
deRefVal valref = do
    gctxt <- ask
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
