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

  -- * Running JSaddle given a JSContextRef
  , runJSaddle
  , run

  -- * Exception Handling
  , syncPoint
  , syncAfter
  , catch
  , bracket
) where

import Prelude hiding (catch, read)
import Control.Monad.Trans.Reader (runReaderT, ask, ReaderT(..))
import Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Exception as E (Exception, catch, bracket)
import Language.Javascript.JSaddle.Types (JSM, runJSaddle, JSContextRef(..))
import Control.Monad.Trans.Reader (ReaderT(..))
#ifdef ghcjs_HOST_OS
import Control.Monad.Trans.Reader (ReaderT(..))
run :: Int -> JSM () -> IO ()
run _port = (`runReaderT` ())

syncPoint :: JSM ()
syncPoint = return ()

syncAfter :: JSM a -> JSM a
syncAfter = id
#else
import Language.Javascript.JSaddle.WebSockets (run, syncPoint, syncAfter)
#endif

-- | Wrapped version of 'E.catch' that runs in a MonadIO that works
--   a bit better with 'JSM'
catch :: E.Exception e
      => JSM b
      -> (e -> JSM b)
      -> JSM b
t `catch` c = do
    r <- ask
    liftIO (runReaderT (syncAfter t) r `E.catch` \e -> runReaderT (c e) r)

-- | Wrapped version of 'E.bracket' that runs in a MonadIO that works
--   a bit better with 'JSM'
bracket :: JSM a -> (a -> JSM b) -> (a -> JSM c) -> JSM c
bracket aquire release f = do
    r <- ask
    liftIO $ E.bracket
        (runReaderT (syncAfter aquire) r)
        (\x -> runReaderT (syncAfter $ release x) r)
        (\x -> runReaderT (syncAfter $ f x) r)

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


