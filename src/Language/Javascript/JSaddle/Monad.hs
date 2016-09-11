{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
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
  , catch
  , bracket

  -- * GUI thread support
  , postGUIAsyncJS
  , postGUISyncJS
) where

import Prelude hiding (catch, read)
import Control.Monad.Trans.Reader (runReaderT, ask, ReaderT(..))
import Language.Javascript.JSaddle.Types
       (JSM, JSVal, MutableJSArray, JSContextRef(..))
import Control.Monad.IO.Class (MonadIO(..))
#ifdef ghcjs_HOST_OS
import GHCJS.Types (isUndefined, isNull)
import qualified JavaScript.Array as Array (create, read)
#else
import Network.WebSockets (Connection)
import Language.Javascript.JSaddle.Native (wrapJSVal)
#endif
import qualified Control.Exception as E (Exception, catch, bracket)
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import Control.Monad (void)
import Control.Concurrent (forkIO)

#ifndef ghcjs_HOST_OS
-- | Post an action to be run in the main GUI thread.
--
-- The current thread blocks until the action completes and the result is
-- returned.
--
postGUISync :: IO a -> IO a
postGUISync = id

-- | Post an action to be run in the main GUI thread.
--
-- The current thread continues and does not wait for the result of the
-- action.
--
postGUIAsync :: IO () -> IO ()
postGUIAsync = void . forkIO
#endif

-- | Wrapped version of 'E.catch' that runs in a MonadIO that works
--   a bit better with 'JSM'
catch :: (MonadIO m, E.Exception e)
      => ReaderT r IO b
      -> (e -> ReaderT r IO b)
      -> ReaderT r m b
t `catch` c = do
    r <- ask
    liftIO (runReaderT t r `E.catch` \e -> runReaderT (c e) r)

-- | Wrapped version of 'E.bracket' that runs in a MonadIO that works
--   a bit better with 'JSM'
bracket :: MonadIO m => ReaderT r IO a -> (a -> ReaderT r IO b) -> (a -> ReaderT r IO c) -> ReaderT r m c
bracket aquire release f = do
    r <- ask
    liftIO $ E.bracket
        (runReaderT aquire r)
        (\x -> runReaderT (release x) r)
        (\x -> runReaderT (f x) r)

{-
-- | Handle JavaScriptCore functions that take a MutableJSArray in order
--   to throw exceptions.
catchval :: (MutableJSArray -> JSM a) -> (JSVal -> JSM a) -> JSM a
catchval f catcher = do
#ifdef ghcjs_HOST_OS
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
-}

#ifdef ghcjs_HOST_OS
runJSaddle :: MonadIO m => w -> JSM a -> m a
runJSaddle _ f = liftIO $ runReaderT f ()
#else
runJSaddle :: MonadIO m => Connection -> JSM a -> m a
runJSaddle connection f = liftIO $
    runReaderT f (JSContextRef connection)
#endif

postGUIAsyncJS :: JSM () -> JSM ()
#ifdef ghcjs_HOST_OS
postGUIAsyncJS = id
#else
postGUIAsyncJS f = do
    r <- ask
    liftIO . postGUIAsync $ runReaderT f r
#endif

postGUISyncJS :: JSM a -> JSM a
#ifdef ghcjs_HOST_OS
postGUISyncJS = id
#else
postGUISyncJS f = do
    r <- ask
    liftIO . postGUISync $ runReaderT f r
#endif

