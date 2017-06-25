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
       (runJSM, askJSM, JSM, JSContextRef(..))
import Data.IORef (readIORef, atomicModifyIORef', newIORef, IORef)
import System.IO.Unsafe (unsafePerformIO)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (MonadIO(..))
import Data.UUID (UUID)

contexts :: IORef [JSContextRef]
contexts = unsafePerformIO $ newIORef []
{-# NOINLINE contexts #-}

addContext :: JSM ()
addContext = do
    ctx <- askJSM
    liftIO $ atomicModifyIORef' contexts $ \c -> (c <> [ctx], ())

removeContext :: MonadIO m => UUID -> m ()
removeContext uuid =
    liftIO $ atomicModifyIORef' contexts $ \c -> (filter ((/= uuid) . contextId) c, ())

runOnAll :: MonadIO m => JSM a -> m [a]
runOnAll f = liftIO (readIORef contexts) >>= mapM (runJSM f)

runOnAll_ :: MonadIO m => JSM a -> m ()
runOnAll_ f = liftIO (readIORef contexts) >>= mapM_ (runJSM f)
