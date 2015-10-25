{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
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
  , Object(..)
  , JSPropertyNameArrayRef(..)
  , JSPropertyAttributes(..)
  , JSContextRef(..)
  , JSStringRef(..)
  , Index(..)
) where

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import GHCJS.Types
import JavaScript.Object.Internal (Object(..))
import JavaScript.Array (MutableJSArray)
import Data.Word (Word(..))
#else
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSValueRef, JSValueRefRef, JSObjectRef, JSStringRef,
        JSContextRef, JSPropertyNameArrayRef)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef (JSPropertyAttributes)
import Foreign.C (CUInt(..))
#endif

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
type JSValueRef  = JSVal
type JSValueRefRef = MutableJSArray
newtype JSPropertyNameArrayRef = JSPropertyNameArrayRef { unJSPropertyNameArrayRef :: JSVal }
type JSPropertyAttributes = Word
type JSContextRef  = ()
type JSStringRef   = JSString
type Index         = Int
#else
type Index = CUInt
newtype Object = Object JSObjectRef
#endif
