{-# LANGUAGE ForeignFunctionInterface
           , TypeSynonymInstances
           , FlexibleInstances
           , DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC (
    module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Monad
  , module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Value
  , module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Object

  , global
  , evaluateScript
  , eval
) where

import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Monad
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Value
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Object

import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSContextRef
       (jscontextgetglobalobject)
import Foreign (nullPtr)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (jsevaluatescript, JSValueRefRef)

global :: JSC JSObjectRef
global = ask >>= (liftIO . jscontextgetglobalobject)

evaluateScript :: MakeStringRef script
               => MakeStringRef url
               => script
               -> JSObjectRef
               -> url
               -> Int
               -> JSValueRefRef
               -> JSC JSValueRef
evaluateScript script this url line exception = do
    gctxt <- ask
    sScript <- makeStringRef script
    sUrl <- makeStringRef url
    liftIO $ jsevaluatescript gctxt sScript this sUrl line exception

eval script = rethrow $ evaluateScript script nullPtr (nullPtr::JSStringRef) 1



