{-# LANGUAGE ForeignFunctionInterface
           , TypeSynonymInstances
           , FlexibleInstances
           , DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC.Monad
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSC.Monad (
    JSC(..)
  , JSContextRef
  , catch
) where

import Prelude hiding (catch)
import Control.Monad.Trans.Reader (runReaderT, ask, ReaderT(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSValueRefRef, JSValueRef, JSContextRef)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign (nullPtr, alloca)
import Foreign.Storable (Storable(..))

type JSC = ReaderT JSContextRef IO

catch :: (JSValueRefRef -> JSC a) -> (JSValueRef -> JSC a) -> JSC a
catch f catcher = do
  gctxt <- ask
  liftIO . alloca $ \pexc -> flip runReaderT gctxt $ do
    liftIO $ poke pexc nullPtr
    result <- f pexc
    exc <- liftIO $ peek pexc
    if exc == nullPtr
        then return result
        else catcher exc


