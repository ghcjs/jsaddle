{-# LANGUAGE ForeignFunctionInterface
           , TypeSynonymInstances
           , FlexibleInstances
           , DeriveDataTypeable
           , ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Object
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Object (
    objSetProperty
  , objGetProperty
  , objCallAsFunction
  , (!)
  , (.!)
  , (#)
  , (<#)
  , function
  , fun
) where

import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSStringRef, JSObjectRef, JSValueRefRef, JSValueRef, JSContextRef)
import Foreign.C (CULong(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
       (jsobjectcallasfunction, jsobjectgetproperty, jsobjectsetproperty,
        JSPropertyAttributes, JSObjectCallAsFunctionCallback,
        jsobjectmakefunctionwithcallback)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Value
       (valMakeUndefined, valToObject, MakeValueRef(..),
        MakeStringRef(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Monad
       (rethrow, JSC)
import Control.Monad.Trans.Reader (runReaderT, ask)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign (peekArray, nullPtr, withArrayLen)
import qualified Control.Exception as E (catch)
import Control.Exception (SomeException)
import qualified Data.Text as T (pack)
import Foreign.Storable (Storable(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
       (jsvaluemakeundefined)

objSetProperty :: (MakeStringRef name, MakeValueRef val)
               => JSObjectRef
               -> name
               -> val
               -> JSPropertyAttributes
               -> JSValueRefRef
               -> JSC ()
objSetProperty this name val attributes exceptions = do
    gctxt <- ask
    sName <- makeStringRef name
    vref <- makeValueRef val
    liftIO $ jsobjectsetproperty gctxt this sName vref attributes exceptions

objGetProperty :: MakeStringRef name
               => JSObjectRef
               -> name
               -> JSValueRefRef
               -> JSC JSValueRef
objGetProperty this name exceptions = do
    gctxt <- ask
    sName <- makeStringRef name
    liftIO $ jsobjectgetproperty gctxt this sName exceptions

objCallAsFunction :: MakeValueRef arg
                  => JSObjectRef
                  -> JSObjectRef
                  -> [arg]
                  -> JSValueRefRef
                  -> JSC JSValueRef
objCallAsFunction function this args exceptions = do
    gctxt <- ask
    rargs <- mapM makeValueRef args
    liftIO $ withArrayLen rargs $ \ largs pargs ->
        jsobjectcallasfunction gctxt function this (fromIntegral largs) pargs exceptions

this ! name = return (this, name)

exp .! name2 = do
    (this1, name1) <- exp
    this2 <- (rethrow $ objGetProperty this1 name1) >>= valToObject
    this2 ! name2

exp # args = do
    (this, name) <- exp
    f <- (rethrow $ objGetProperty this name) >>= valToObject
    rethrow $ objCallAsFunction f this args

exp <# val = do
    (this, name) <- exp
    rethrow $ objSetProperty this name val 0

type JSObjectCallAsFunctionCallback' =
       JSContextRef
    -> JSValueRef
    -> JSValueRef
    -> CULong
    -> JSValueRefRef
    -> JSValueRefRef
    -> IO JSValueRef

foreign import ccall "wrapper"
  mkJSObjectCallAsFunctionCallback :: JSObjectCallAsFunctionCallback' -> IO JSObjectCallAsFunctionCallback

function :: (MakeStringRef name, MakeValueRef result)
         => name
         -> (JSValueRef -> JSValueRef -> [JSValueRef] -> JSC result)
         -> JSC JSObjectRef
function name f = do
    gctxt <- ask
    callback <- liftIO $ mkJSObjectCallAsFunctionCallback wrap
    sName <- makeStringRef name
    liftIO $ jsobjectmakefunctionwithcallback gctxt sName callback
  where
    wrap ctx fobj this argc argv exception = do
            args <- peekArray (fromIntegral argc) argv
            flip runReaderT ctx $ do
                f fobj this args >>= makeValueRef
      `E.catch` \(e :: SomeException) -> do
            str <- runReaderT (makeValueRef $ show e) ctx
            poke exception str
            jsvaluemakeundefined ctx

data JSCallAsFunction result = JSCallAsFunction (JSValueRef -> JSValueRef -> [JSValueRef] -> JSC result)

fun = JSCallAsFunction

instance MakeValueRef result => MakeValueRef (JSCallAsFunction result) where
    makeValueRef (JSCallAsFunction f) = function (nullPtr::JSStringRef) f

