{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.WebSockets
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Debug (
    contexts
  , addContext
  , removeContext
  , runOnAll
  , runOnAll_
) where

import Language.Javascript.JSaddle
       (runJSM, JSM, JSContextRef(..), askJSM)
import Data.IORef (readIORef, atomicModifyIORef', newIORef, IORef)
import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function (on)

contexts :: IORef [JSContextRef]
contexts = unsafePerformIO $ newIORef []
{-# NOINLINE contexts #-}

addContext :: JSM ()
addContext = do
    ctx <- askJSM
    liftIO $ atomicModifyIORef' contexts $ \c -> (c <> [ctx], ())

removeContext :: MonadIO m => JSContextRef -> m ()
removeContext c =
    liftIO $ atomicModifyIORef' contexts $ \cs -> (filter (((/=) `on` _jsContextRef_nextRefId) c) cs, ())

runOnAll :: MonadIO m => JSM a -> m [a]
runOnAll f = liftIO (readIORef contexts) >>= mapM (runJSM f)

runOnAll_ :: MonadIO m => JSM a -> m ()
runOnAll_ f = liftIO (readIORef contexts) >>= mapM_ (runJSM f)
