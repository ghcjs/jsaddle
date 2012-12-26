{-# LANGUAGE ForeignFunctionInterface
           , TypeSynonymInstances
           , FlexibleInstances
           , DeriveDataTypeable
           , TemplateHaskell
           , QuasiQuotes #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSC (
    module Language.Javascript.JSC.Monad
  , module Language.Javascript.JSC.Value
  , module Language.Javascript.JSC.Object
  , module Language.Javascript.JSC.JMacro

  , evaluateScript
  , eval
) where

import Language.Javascript.JSC.Monad
import Language.Javascript.JSC.Value
import Language.Javascript.JSC.Object
import Language.Javascript.JSC.JMacro

import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSContextRef
       (jscontextgetglobalobject)
import Foreign (nullPtr)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (jsevaluatescript, JSValueRefRef)

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

