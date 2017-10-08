{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
#ifdef ghcjs_HOST_OS
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-dodgy-imports #-}
#endif
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

#ifndef ghcjs_HOST_OS
import Control.Monad.Trans.Reader (runReaderT, ask)
#endif
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch (catch, bracket)
import Language.Javascript.JSaddle.Types (JSM(..), MonadJSM, liftJSM, JSContextRef)
import Language.Javascript.JSaddle.Run (syncPoint, syncAfter, waitForAnimationFrame, nextAnimationFrame)

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
