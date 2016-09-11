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
    JSVal(..)
  , MutableJSArray(..)
  , Object(..)
  , JSPropertyNameArray(..)
  , JSPropertyAttributes(..)
  , JSContextRef(..)
  , JSString(..)
  , Index
  , Nullable(..)
  , JSM
#ifndef ghcjs_HOST_OS
  , JSValueReceived(..)
  , JSValueForSend(..)
  , JSStringReceived(..)
  , JSStringForSend(..)
  , JSObjectForSend(..)
#endif
) where

import Control.Monad.Trans.Reader (ReaderT(..))
#ifdef ghcjs_HOST_OS
import GHCJS.Types
import JavaScript.Object.Internal (Object(..))
import JavaScript.Array (MutableJSArray)
import Data.Word (Word(..))
import GHCJS.Nullable (Nullable(..))
#else
import Network.WebSockets (Connection)
import Data.Text (Text)
#endif

#ifdef ghcjs_HOST_OS
newtype JSPropertyNameArray = JSPropertyNameArray { unJSPropertyNameArrayRef :: JSVal }
type JSPropertyAttributes = Word
type JSContextRef  = ()
type Index         = Int
#else
newtype JSValueReceived = JSValueReceived Int
newtype JSValueForSend = JSValueForSend Int
newtype JSVal = JSVal Int
newtype MutableJSArray = MutableJSArray Int
newtype JSPropertyNameArray = JSPropertyNameArray Int
newtype JSPropertyAttributes = JSPropertyAttributes Word
type Index = Int
newtype JSObjectForSend = JSObjectForSend JSValueForSend
newtype Object = Object JSVal
newtype JSStringReceived = JSStringReceived Int
newtype JSStringForSend = JSStringForSend Int
newtype JSString = JSString Int
newtype Nullable a = Nullable a
data JSContextRef = JSContextRef Connection
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


