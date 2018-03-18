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
  , MonadJSM
  , liftJSM
  , askJSM
  , runJSM
  , syncPoint
  , syncAfter
  , waitForAnimationFrame
  , nextAnimationFrame
  , animationFrameHandlers

  -- * Exception Handling
  , catch
  , bracket
) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch (catch, bracket)
import Language.Javascript.JSaddle.Types (JSM(..), MonadJSM, liftJSM, askJSM, JSContextRef, runJSM)
import Control.Concurrent.MVar (MVar, newMVar)
import System.IO.Unsafe (unsafePerformIO)

{-# DEPRECATED syncPoint "Use 'liftIO $ return ()' instead." #-}
syncPoint :: MonadIO m => m ()
syncPoint = liftIO $ return ()

syncAfter :: Applicative m => m ()
syncAfter = pure () --TODO: What should this do?

waitForAnimationFrame :: m () -> m ()
waitForAnimationFrame = id --TODO: What should this do?

nextAnimationFrame :: m () -> m ()
nextAnimationFrame = id --TODO: What should this do?

--TODO: Get rid of this
animationFrameHandlerVar :: MVar [Double -> JSM ()]
animationFrameHandlerVar = unsafePerformIO $ newMVar []
{-# NOINLINE animationFrameHandlerVar #-}

animationFrameHandlers :: JSContextRef -> MVar [Double -> JSM ()]
animationFrameHandlers = return animationFrameHandlerVar
