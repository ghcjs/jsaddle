{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Run
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Run (
  -- * Running JSM
#ifndef ghcjs_HOST_OS
  -- * Functions used to implement JSaddle using JSON messaging
    runJS
  , newJson
  , sync
  , lazyValResult
  , freeSyncCallback
  , newSyncCallback'
  , newSyncCallback''
  , callbackToSyncFunction
  , callbackToAsyncFunction
  , syncPoint
  , getProperty
  , setProperty
  , getJson
  , getJsonLazy
  , callAsFunction'
  , callAsConstructor'
  , globalRef
#endif
) where

#ifdef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Types (JSM, syncPoint, syncAfter)
import qualified JavaScript.Web.AnimationFrame as GHCJS
       (waitForAnimationFrame)
#else
import Control.Monad (when, join)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar (writeTVar, readTVar, newTVarIO)
import Control.Concurrent.MVar
       (putMVar, takeMVar, newMVar, newEmptyMVar, modifyMVar, modifyMVar_, swapMVar)

import Data.Monoid ((<>))
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import Data.IORef (newIORef)

import Language.Javascript.JSaddle.Types
--TODO: Handle JS exceptions
import Data.Foldable (forM_)
import System.IO.Unsafe
import Language.Javascript.JSaddle.Monad (syncPoint)
#endif

-- | The RefId of the global object
globalRefId :: RefId
globalRefId = RefId 1

globalRef :: Ref
globalRef = unsafePerformIO $ Ref <$> newIORef globalRefId
{-# NOINLINE globalRef #-}

-- | The first dynamically-allocated RefId
initialRefId :: RefId
initialRefId = RefId 2

runJS
  :: (Req ValId RefId -> IO ()) -- ^ Send a request to the JS engine; we assume that requests are performed in the order they are sent; requests received while in a synchronous block must not be processed until the synchronous block ends (i.e. until the JS side receives the final value yielded back from the synchronous block)
  -> IO ( Rsp -> IO () -- Responses must be able to continue coming in as a sync block runs, or else the caller must be careful to ensure that sync blocks are only run after all outstanding responses have been processed
        , SyncCallbackId -> ValId -> [ValId] -> IO [Either ValId (Req ValId RefId)] -- The input valIds here must always be allocated on the JS side
        , IO [Either ValId (Req ValId RefId)]
        , JSContextRef
        )
runJS sendReqAsync = do
  nextRefId <- newTVarIO initialRefId
  nextGetJsonReqId <- newTVarIO $ GetJsonReqId 1
  getJsonReqs <- newTVarIO M.empty
  nextSyncCallbackId <- newTVarIO $ SyncCallbackId 1
  syncCallbacks <- newTVarIO M.empty
  pendingResults <- newTVarIO M.empty
  yieldAccumVar <- newMVar [] -- Accumulates results that need to be yielded
  yieldReadyVar <- newEmptyMVar -- Filled when there is at least one item in yieldAccumVar
  -- Each value in the map corresponds to a value ready to be returned from the sync frame corresponding to its key
  -- INVARIANT: \(depth, readyFrames) -> all (< depth) $ M.keys readyFrames
  syncState <- newMVar (0, M.empty)
  let enqueueYieldVal val = modifyMVar_ yieldAccumVar $ \old -> do
        let new = val : old
        when (null old) $ do
          putMVar yieldReadyVar ()
        return new
      enterSyncFrame = modifyMVar syncState $ \(oldDepth, readyFrames) -> do
        let !newDepth = succ oldDepth
        return ((newDepth, readyFrames), newDepth)
      exitSyncFrame myDepth myRetVal = modifyMVar_ syncState $ \(oldDepth, oldReadyFrames) -> case oldDepth `compare` myDepth of
        LT -> error "should be impossible: trying to return from deeper sync frame than the current depth"
        -- We're the top frame, so yield our value to the caller
        EQ -> do
          let yieldAllReady :: Int -> Map Int JSVal -> JSVal -> IO (Int, Map Int JSVal)
              yieldAllReady depth readyFrames retVal = do
                -- Even though the valId is escaping, this is safe because we know that our yielded value will go out before any potential FreeVal request could go out
                flip runJSM env $ withJSValId retVal $ \retValId -> do
                  JSM $ liftIO $ enqueueYieldVal $ Left retValId

                let !nextDepth = pred depth
                case M.maxViewWithKey readyFrames of
                  -- The parent frame is also ready to yield
                  Just ((k, nextRetVal), nextReadyFrames)
                    | k == nextDepth
                      -> yieldAllReady nextDepth nextReadyFrames nextRetVal
                  _ -> return (nextDepth, readyFrames)
          yieldAllReady oldDepth oldReadyFrames myRetVal
        -- We're not the top frame, so just store our value so it can be yielded later
        GT -> do
          let !newReadyFrames = M.insertWith (error "should be impossible: trying to return from a sync frame that has already returned") myDepth myRetVal oldReadyFrames
          return (oldDepth, newReadyFrames)
      yield = do
        takeMVar yieldReadyVar
        reverse <$> swapMVar yieldAccumVar []
      processRsp = \case
        Rsp_GetJson getJsonReqId val -> do
          reqs <- atomically $ do
            reqs <- readTVar getJsonReqs
            writeTVar getJsonReqs $! M.delete getJsonReqId reqs
            return reqs
          forM_ (M.lookup getJsonReqId reqs) $ \resultVar -> do
            putMVar resultVar val
        Rsp_Result refId primVal -> do
          mResultVar <- atomically $ do
            resultVars <- readTVar pendingResults
            let mResultVar = M.lookup refId resultVars
            when (isJust mResultVar) $ do
              writeTVar pendingResults $! M.delete refId resultVars
            return mResultVar
          forM_ mResultVar $ \resultVar -> do
            putMVar resultVar primVal
        --TODO: Rename "SyncCallback" stuff to just "Callback"
        Rsp_CallAsync callbackId this args -> do
          mCallback <- fmap (M.lookup callbackId) $ atomically $ readTVar syncCallbacks
          case mCallback of
            Just callback -> do
              _ <- forkIO $ flip runJSM env $ do
                _ <- join $ callback <$> wrapJSVal this <*> traverse wrapJSVal args
                return ()
              return ()
            Nothing -> error $ "callback " <> show callbackId <> " called, but does not exist"
      runSyncCallback syncCallbackId this args = do
        mSyncCallback <- fmap (M.lookup syncCallbackId) $ atomically $ readTVar syncCallbacks
        case mSyncCallback of
          Just (syncCallback :: JSVal -> [JSVal] -> JSM JSVal) -> do
            --TODO: Only use use the yield var for requests that someone might block on; e.g., don't do it for FreeVal; however, FreeVal must still wait until the synchronous block has finished, because otherwise it might free the return value of the synchronous block; however, we also don't want to prevent all cleanup in the event of a long sync block
            myDepth <- enterSyncFrame
            _ <- forkIO $ do
              result <- flip runJSM (env { _jsContextRef_sendReq = enqueueYieldVal . Right }) $ join $ syncCallback
                <$> wrapJSVal this
                <*> traverse wrapJSVal args --TODO: Handle exceptions that occur within the syncCallback
              exitSyncFrame myDepth result
            yield
          Nothing -> error $ "sync callback " <> show syncCallbackId <> " called, but does not exist"
      continueSyncCallback = yield
      env = JSContextRef
        { _jsContextRef_sendReq = sendReqAsync
        , _jsContextRef_nextRefId = nextRefId
        , _jsContextRef_nextGetJsonReqId = nextGetJsonReqId
        , _jsContextRef_getJsonReqs = getJsonReqs
        , _jsContextRef_nextSyncCallbackId = nextSyncCallbackId
        , _jsContextRef_syncCallbacks = syncCallbacks
        , _jsContextRef_pendingResults = pendingResults
        }
  return (processRsp, runSyncCallback, continueSyncCallback, env)
