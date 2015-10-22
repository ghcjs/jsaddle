{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

module Language.Javascript.JSaddle.Types where

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import GHCJS.Types
import JavaScript.Array
import Data.Word (Word(..))
#else
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSValueRef, JSValueRefRef, JSObjectRef, JSStringRef,
        JSContextRef, JSPropertyNameArrayRef)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef (JSPropertyAttributes)
import Foreign.C (CUInt(..))
#endif

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
--data ValueOrObject
--data Object
--data PropertyNameArray
newtype JSValueRef             = JSValueRef             { fromJSValueRef             :: JSRef }
newtype JSObjectRef            = JSObjectRef            { fromJSObjectRef            :: JSRef }
newtype JSPropertyNameArrayRef = JSPropertyNameArrayRef { fromJSPropertyNameArrayRef :: JSRef }

instance Show JSValueRef             where show _ = "JSValueRef"
instance Show JSObjectRef            where show _ = "JSObjectRef"
instance Show JSPropertyNameArrayRef where show _ = "JSPropertyNameArrayRef"
--type JSValueRef    = JSRef ValueOrObject
--type JSValueRefRef = JSArray ValueOrObject
type JSValueRefRef = MutableJSArray
--type JSObjectRef   = JSRef ValueOrObject
--type JSPropertyNameArrayRef = JSRef PropertyNameArray
type JSPropertyAttributes = Word
type JSContextRef  = ()
type JSStringRef   = JSString
type Index         = Int

class    RefCast a           b           where castRef :: a -> b
instance RefCast JSValueRef  JSRef       where castRef = fromJSValueRef
instance RefCast JSValueRef  JSObjectRef where castRef = JSObjectRef . castRef
instance RefCast JSRef       JSValueRef  where castRef = JSValueRef
instance RefCast JSObjectRef JSValueRef  where castRef = JSValueRef . castRef
instance RefCast JSObjectRef JSRef       where castRef = fromJSObjectRef
instance RefCast JSRef       JSObjectRef where castRef = JSObjectRef


instance Eq JSObjectRef
--instance Show (JSRef ValueOrObject)
--instance Eq (JSRef ValueOrObject)
#else
type Index = CUInt
#endif
