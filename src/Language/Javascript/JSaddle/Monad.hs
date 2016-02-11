{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Monad
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | JSM monad keeps track of the JavaScript context
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Monad (
  -- * Types
    JSM
  , JSContextRef

  -- * Running JSaddle given a DOM Window
  , runJSaddle

  -- * Exception Handling
  , catchval
  , catch
  , bracket

  -- * GUI thread support
  , postGUIAsyncJS
  , postGUISyncJS
) where

import Prelude hiding (catch, read)
import Control.Monad.Trans.Reader (runReaderT, ask, ReaderT(..))
import Language.Javascript.JSaddle.Types
       (JSM, JSVal, MutableJSArray, JSContextRef)
import Control.Monad.IO.Class (MonadIO(..))
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import GHCJS.Types (isUndefined, isNull)
import qualified JavaScript.Array as Array (create, read)
#else
import Language.Javascript.JSaddle.Native (makeNewJSVal)
import Foreign (nullPtr, alloca)
import Foreign.Storable (Storable(..))
import Graphics.UI.Gtk.WebKit.Types
       (WebView(..))
import Graphics.UI.Gtk.WebKit.WebView
       (webViewGetMainFrame)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.WebFrame
       (webFrameGetGlobalContext)
import Graphics.UI.Gtk.General.General (postGUIAsync, postGUISync)
#endif
import qualified Control.Exception as E (Exception, catch, bracket)

-- | Wrapped version of 'E.catch' that runs in a MonadIO that works
--   a bit better with 'JSM'
catch :: (MonadIO m, E.Exception e)
      => ReaderT r IO b
      -> (e -> ReaderT r IO b)
      -> ReaderT r m b
t `catch` c = do
    r <- ask
    liftIO (runReaderT t r `E.catch` \e -> runReaderT (c e) r)
{-# INLINE catch #-}

-- | Wrapped version of 'E.bracket' that runs in a MonadIO that works
--   a bit better with 'JSM'
bracket :: MonadIO m => ReaderT r IO a -> (a -> ReaderT r IO b) -> (a -> ReaderT r IO c) -> ReaderT r m c
bracket aquire release f = do
    r <- ask
    liftIO $ E.bracket
        (runReaderT aquire r)
        (\x -> runReaderT (release x) r)
        (\x -> runReaderT (f x) r)
{-# INLINE bracket #-}

-- | Handle JavaScriptCore functions that take a MutableJSArray in order
--   to throw exceptions.
catchval :: (MutableJSArray -> JSM a) -> (JSVal -> JSM a) -> JSM a
catchval f catcher = do
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
    pexc   <- liftIO Array.create
    result <- f pexc
    exc    <- liftIO $ Array.read 0 pexc
    if isUndefined exc || isNull exc
        then return result
        else catcher exc
#else
    gctxt <- ask
    liftIO . alloca $ \pexc -> flip runReaderT gctxt $ do
        liftIO $ poke pexc nullPtr
        result <- f pexc
        exc <- liftIO $ peek pexc
        if exc == nullPtr
            then return result
            else makeNewJSVal exc >>= catcher
#endif

#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
runJSaddle :: w -> JSM a -> IO a
runJSaddle _ f = runReaderT f ()
#else
runJSaddle :: WebView -> JSM a -> IO a
runJSaddle webView f = do
    gctxt <- webViewGetMainFrame webView >>= webFrameGetGlobalContext
    runReaderT f gctxt
#endif
{-# INLINE runJSaddle #-}

postGUIAsyncJS :: JSM () -> JSM ()
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
postGUIAsyncJS = id
#else
postGUIAsyncJS f = do
    r <- ask
    liftIO . postGUIAsync $ runReaderT f r
#endif
{-# INLINE postGUIAsyncJS #-}

postGUISyncJS :: JSM a -> JSM a
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
postGUISyncJS = id
#else
postGUISyncJS f = do
    r <- ask
    liftIO . postGUISync $ runReaderT f r
#endif
{-# INLINE postGUISyncJS #-}

