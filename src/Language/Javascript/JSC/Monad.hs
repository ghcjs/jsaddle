{-# LANGUAGE ForeignFunctionInterface
           , TypeSynonymInstances
           , FlexibleInstances
           , DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Monad
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
  , JSException(..)
  , catch
  , rethrow
) where

import qualified Control.Exception as E (throwIO, Exception)
import Control.Monad.Trans.Reader (runReaderT, ask, ReaderT(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSValueRefRef, JSValueRef, JSContextRef)
import Data.Typeable (Typeable)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign (nullPtr, alloca)
import Foreign.Storable (Storable(..))

type JSC = ReaderT JSContextRef IO

newtype JSException = JSException JSValueRef deriving (Show, Typeable)

instance E.Exception JSException

catch :: (JSValueRefRef -> JSC a) -> (JSValueRef -> JSC a) -> JSC a
catch f catcher = do
  gctxt <- ask
  liftIO . alloca $ \pexc -> flip runReaderT gctxt $ do
    result <- f pexc
    exc <- liftIO $ peek pexc
    if exc == nullPtr
        then return result
        else catcher exc

rethrow :: (JSValueRefRef -> JSC a) -> JSC a
rethrow = flip catch $ liftIO . E.throwIO . JSException


