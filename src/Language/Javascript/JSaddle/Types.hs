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
    JSVal
  , MutableJSArray
  , Object(..)
  , JSPropertyNameArray
  , JSPropertyAttributes
  , JSContextRef
  , JSString
  , Index
  , Nullable(..)
  , JSM
) where

import Control.Monad.Trans.Reader (ReaderT(..))
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import GHCJS.Types
import JavaScript.Object.Internal (Object(..))
import JavaScript.Array (MutableJSArray)
import Data.Word (Word(..))
import GHCJS.Nullable (Nullable(..))
#else
import Foreign.ForeignPtr (ForeignPtr)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (OpaqueJSString, JSValueRefRef,
        JSContextRef, JSPropertyNameArrayRef, OpaqueJSValue)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef (JSPropertyAttributes)
import Foreign.C (CUInt(..))
#endif

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
newtype JSPropertyNameArray = JSPropertyNameArray { unJSPropertyNameArrayRef :: JSVal }
type JSPropertyAttributes = Word
type JSContextRef  = ()
type Index         = Int
#else
type JSVal = ForeignPtr OpaqueJSValue
type MutableJSArray = JSValueRefRef
type JSPropertyNameArray = JSPropertyNameArrayRef
type Index = CUInt
newtype Object = Object (ForeignPtr OpaqueJSValue)
type JSString = ForeignPtr OpaqueJSString
newtype Nullable a = Nullable a
#endif

-- | The @JSM@ monad keeps track of the JavaScript context.
--
-- Given a @JSM@ function and a 'JSContextRef' you can run the
-- function like this...
--
-- > runReaderT jsmFunction javaScriptContext
--
-- For an example of how to set up WebKitGTK+ see tests/TestJSaddle.hs
type JSM = ReaderT JSContextRef IO


