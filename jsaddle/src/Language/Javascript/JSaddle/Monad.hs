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
    JSM(..)
  , JSContextRef
  , MonadJSM
  , liftJSM

  -- * Running JavaScript in a JavaScript context
  , askJSM
  , run
  , runJSM
  , runJSaddle

  -- * Syncronizing with the JavaScript context
  , syncPoint
  , syncAfter
  , waitForAnimationFrame
  , nextAnimationFrame

  -- * Exception Handling
  , catch
  , bracket
) where

import Prelude hiding (catch, read)
import Control.Monad.Trans.Reader (runReaderT, ask, ReaderT(..))
import Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Exception as E (Exception, catch, bracket)
import Language.Javascript.JSaddle.Types (JSM(..), MonadJSM, liftJSM, JSContextRef(..))
import Language.Javascript.JSaddle.WebSockets (run, syncPoint, syncAfter, waitForAnimationFrame, nextAnimationFrame)

-- | Gets the JavaScript context from the monad
askJSM :: MonadJSM m => m JSContextRef
#ifdef ghcjs_HOST_OS
askJSM = return ()
#else
askJSM = liftJSM $ JSM ask
#endif

-- | Runs a 'JSM' JavaScript function in a given JavaScript context.
runJSM :: MonadIO m => JSM a -> JSContextRef -> m a
#ifdef ghcjs_HOST_OS
runJSM f = liftIO . const f
#else
runJSM f = liftIO . runReaderT (unJSM f)
#endif

-- | Alternative version of 'runJSM'
runJSaddle :: MonadIO m => JSContextRef -> JSM a -> m a
runJSaddle = flip runJSM


-- | Wrapped version of 'E.catch' that runs in a MonadIO that works
--   a bit better with 'JSM'
catch :: E.Exception e
      => JSM b
      -> (e -> JSM b)
      -> JSM b
t `catch` c = do
    r <- askJSM
    liftIO (runJSM (syncAfter t) r `E.catch` \e -> runJSM (c e) r)

-- | Wrapped version of 'E.bracket' that runs in a MonadIO that works
--   a bit better with 'JSM'
bracket :: JSM a -> (a -> JSM b) -> (a -> JSM c) -> JSM c
bracket aquire release f = do
    r <- askJSM
    liftIO $ E.bracket
        (runJSM (syncAfter aquire) r)
        (\x -> runJSM (syncAfter $ release x) r)
        (\x -> runJSM (syncAfter $ f x) r)

