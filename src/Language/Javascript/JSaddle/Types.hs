{-# LANGUAGE CPP, FlexibleInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Properties
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Types (
    JSValueRef(..)
  , JSValueRefRef(..)
  , JSObjectRef(..)
  , JSPropertyNameArrayRef(..)
  , JSPropertyAttributes(..)
  , JSContextRef(..)
  , JSStringRef(..)
  , Index(..)
) where

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import GHCJS.Types
import Data.Word (Word(..))
#else
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSValueRef, JSValueRefRef, JSObjectRef, JSStringRef,
        JSContextRef, JSPropertyNameArrayRef)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef (JSPropertyAttributes)
import Foreign.C (CUInt(..))
#endif

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
data ValueOrObject
data Object
data PropertyNameArray
type JSValueRef    = JSRef ValueOrObject
type JSValueRefRef = JSArray ValueOrObject
type JSObjectRef   = JSRef ValueOrObject
type JSPropertyNameArrayRef = JSRef PropertyNameArray
type JSPropertyAttributes = Word
type JSContextRef  = ()
type JSStringRef   = JSString
type Index         = Int

instance Show (JSRef ValueOrObject)
instance Eq (JSRef ValueOrObject)
#else
type Index = CUInt
#endif
