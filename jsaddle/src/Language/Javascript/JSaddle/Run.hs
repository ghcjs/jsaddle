{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
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
    syncPoint
  , syncAfter
  , waitForAnimationFrame
  , nextAnimationFrame
  , enableLogging
#ifndef ghcjs_HOST_OS
  -- * Functions used to implement JSaddle using JSON messaging
  , runJavaScript
  , AsyncCommand(..)
  , Command(..)
  , Result(..)
  , sendCommand
  , sendLazyCommand
  , sendAsyncCommand
  , wrapJSVal
#endif
) where

#ifdef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Types (JSM)
import qualified JavaScript.Web.AnimationFrame as GHCJS
       (waitForAnimationFrame)
#else
import Control.Exception (throwIO, evaluate)
import Control.Monad (void, when, zipWithM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (ask, runReaderT)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.STM.TChan
       (tryReadTChan, TChan, readTChan, writeTChan, newTChanIO)
import Control.Concurrent.STM.TVar
       (writeTVar, readTVar, readTVarIO, modifyTVar', newTVarIO)
import Control.Concurrent.MVar
       (tryTakeMVar, MVar, putMVar, takeMVar, newMVar, newEmptyMVar, readMVar, modifyMVar)

import System.IO.Unsafe (unsafeInterleaveIO)
import System.Mem.Weak (addFinalizer)
import System.Random

import GHC.Base (IO(..), mkWeak#)
import GHC.Conc (ThreadId(..))
import Data.Monoid ((<>))
import qualified Data.Text as T (unpack, pack)
import qualified Data.Map as M (lookup, delete, insert, empty, size)
import qualified Data.Set as S (empty, member, insert, delete)
import Data.Time.Clock (getCurrentTime,diffUTCTime)
import Data.IORef (newIORef, atomicWriteIORef, readIORef)

import Language.Javascript.JSaddle.Types
       (Command(..), AsyncCommand(..), Result(..), BatchResults(..), Results(..), JSContextRef(..), JSVal(..),
        Object(..), JSValueReceived(..), JSM(..), Batch(..), JSValueForSend(..))
import Language.Javascript.JSaddle.Exception (JSException(..))
import Control.DeepSeq (deepseq)
import GHC.Stats (getGCStatsEnabled, getGCStats, GCStats(..))
import Data.Foldable (forM_)
#endif

-- | Enable (or disable) JSaddle logging
enableLogging :: Bool -> JSM ()
#ifdef ghcjs_HOST_OS
enableLogging _ = return ()
#else
enableLogging v = do
    f <- doEnableLogging <$> JSM ask
    liftIO $ f v
#endif

-- | Forces execution of pending asyncronous code
syncPoint :: JSM ()
#ifdef ghcjs_HOST_OS
syncPoint = return ()
#else
syncPoint = do
    SyncResult <- sendCommand Sync
    return ()
#endif

-- | Forces execution of pending asyncronous code after performing `f`
syncAfter :: JSM a -> JSM a
#ifdef ghcjs_HOST_OS
syncAfter = id
#else
syncAfter f = do
    result <- f
    syncPoint
    return result
#endif

-- | On GHCJS this is 'JavaScript.Web.AnimationFrame.waitForAnimationFrame'.
--   On GHC it will delay the execution of the current batch of asynchronous
--   command when they are sent to JavaScript.  It will not delay the Haskell
--   code execution.  The time returned will be based on the Haskell clock
--   (not the JavaScript clock).
waitForAnimationFrame :: JSM Double
#ifdef ghcjs_HOST_OS
waitForAnimationFrame = GHCJS.waitForAnimationFrame
#else
waitForAnimationFrame = do
    -- We can't get the timestamp from requestAnimationFrame so this will have to do
    start <- startTime <$> JSM ask
    now <- liftIO getCurrentTime
    void $ sendLazyCommand SyncWithAnimationFrame
    return $ realToFrac (diffUTCTime now start)
#endif

-- | Tries to executes the given code in the next animation frame callback.
--   Avoid synchronous opperations where possible.
nextAnimationFrame :: (Double -> JSM a) -> JSM a
nextAnimationFrame f = do
    t <- waitForAnimationFrame
    syncAfter (f t)

#ifndef ghcjs_HOST_OS
sendCommand :: Command -> JSM Result
sendCommand cmd = do
    s <- doSendCommand <$> JSM ask
    liftIO $ s cmd

sendLazyCommand :: (JSValueForSend -> AsyncCommand) -> JSM JSVal
sendLazyCommand cmd = do
    nextRefTVar <- nextRef <$> JSM ask
    n <- liftIO . atomically $ do
        n <- subtract 1 <$> readTVar nextRefTVar
        writeTVar nextRefTVar $! n
        return n
    s <- doSendAsyncCommand <$> JSM ask
    liftIO $ s (cmd $ JSValueForSend n)
    wrapJSVal (JSValueReceived n)

sendAsyncCommand :: AsyncCommand -> JSM ()
sendAsyncCommand cmd = do
    s <- doSendAsyncCommand <$> JSM ask
    liftIO $ s cmd

runJavaScript :: (Batch -> IO ()) -> JSM () -> IO (Results -> IO (), Results -> IO Batch, IO ())
runJavaScript sendBatch entryPoint = do
    contextId' <- randomIO
    startTime' <- getCurrentTime
    recvMVar <- newEmptyMVar
    lastAsyncBatch <- newEmptyMVar
    commandChan <- newTChanIO
    callbacks <- newTVarIO M.empty
    nextRef' <- newTVarIO 0
    finalizerThreads' <- newMVar S.empty
    animationFrameHandlers' <- newMVar []
    loggingEnabled <- newIORef False
    let ctx = JSContextRef {
            contextId = contextId'
          , startTime = startTime'
          , doSendCommand = \cmd -> cmd `deepseq` do
                result <- newEmptyMVar
                atomically $ writeTChan commandChan (Right (cmd, result))
                unsafeInterleaveIO $
                    takeMVar result >>= \case
                        (ThrowJSValue (JSValueReceived v)) -> throwIO $ JSException (JSVal v)
                        r -> return r
          , doSendAsyncCommand = \cmd -> cmd `deepseq` atomically (writeTChan commandChan $ Left cmd)
          , addCallback = \(Object (JSVal val)) cb -> atomically $ modifyTVar' callbacks (M.insert val cb)
          , nextRef = nextRef'
          , doEnableLogging = atomicWriteIORef loggingEnabled
          , finalizerThreads = finalizerThreads'
          , animationFrameHandlers = animationFrameHandlers'
          }
        processResults :: Bool -> Results -> IO ()
        processResults syncCallbacks = \case
            (ProtocolError err) -> error $ "Protocol error : " <> T.unpack err
            (Callback n br (JSValueReceived fNumber) f this a) -> do
                putMVar recvMVar (n, br)
                f' <- runReaderT (unJSM $ wrapJSVal f) ctx
                this' <- runReaderT  (unJSM $ wrapJSVal this) ctx
                args <- runReaderT (unJSM $ mapM wrapJSVal a) ctx
                logInfo (("Call " <> show fNumber <> " ") <>)
                (M.lookup fNumber <$> liftIO (readTVarIO callbacks)) >>= \case
                    Nothing -> liftIO $ putStrLn "Callback called after it was freed"
                    Just cb -> void . forkIO $ do
                        runReaderT (unJSM $ cb f' this' args) ctx
                        when syncCallbacks $
                            doSendAsyncCommand ctx EndSyncBlock
            Duplicate nBatch nExpected -> do
                putStrLn $ "Error : Unexpected Duplicate. syncCallbacks=" <> show syncCallbacks <>
                    " nBatch=" <> show nBatch <> " nExpected=" <> show nExpected
                void $ doSendCommand ctx Sync
            BatchResults n br -> putMVar recvMVar (n, br)
        asyncResults :: Results -> IO ()
        asyncResults results =
            void . forkIO $ processResults False results
        syncResults :: Results -> IO Batch
        syncResults results = do
            void . forkIO $ processResults True results
            readMVar lastAsyncBatch
        logInfo s =
            readIORef loggingEnabled >>= \case
                True -> do
                    currentBytesUsedStr <- getGCStatsEnabled >>= \case
                        True  -> show . currentBytesUsed <$> getGCStats
                        False -> return "??"
                    cbCount <- M.size <$> readTVarIO callbacks
                    putStrLn . s $ "M " <> currentBytesUsedStr <> " CB " <> show cbCount <> " "
                False -> return ()
    _ <- forkIO . numberForeverFromM_ 1 $ \nBatch ->
        readBatch nBatch commandChan >>= \case
            (batch@(Batch cmds _ _), resultMVars) -> do
                logInfo (\x -> "Sync " <> x <> show (length cmds, last cmds))
                _ <- tryTakeMVar lastAsyncBatch
                putMVar lastAsyncBatch batch
                sendBatch batch
                takeResult recvMVar nBatch >>= \case
                    (n, _) | n /= nBatch -> error $ "Unexpected jsaddle results (expected batch " <> show nBatch <> ", got batch " <> show n <> ")"
                    (_, Success callbacksToFree results)
                           | length results /= length resultMVars -> error "Unexpected number of jsaddle results"
                           | otherwise -> do
                        zipWithM_ putMVar resultMVars results
                        forM_ callbacksToFree $ \(JSValueReceived val) ->
                            atomically (modifyTVar' callbacks (M.delete val))
                    (_, Failure callbacksToFree results exception) -> do
                        -- The exception will only be rethrown in Haskell if/when one of the
                        -- missing results (if any) is evaluated.
                        putStrLn "A JavaScript exception was thrown! (may not reach Haskell code)"
                        zipWithM_ putMVar resultMVars $ results <> repeat (ThrowJSValue exception)
                        forM_ callbacksToFree $ \(JSValueReceived val) ->
                            atomically (modifyTVar' callbacks (M.delete val))
    return (asyncResults, syncResults, runReaderT (unJSM entryPoint) ctx)
  where
    numberForeverFromM_ :: (Monad m, Enum n) => n -> (n -> m a) -> m ()
    numberForeverFromM_ !n f = do
      _ <- f n
      numberForeverFromM_ (succ n) f
    takeResult recvMVar nBatch =
        takeMVar recvMVar >>= \case
            (n, _) | n < nBatch -> takeResult recvMVar nBatch
            r -> return r
    readBatch :: Int -> TChan (Either AsyncCommand (Command, MVar Result)) -> IO (Batch, [MVar Result])
    readBatch nBatch chan = do
        first <- atomically $ readTChan chan -- We want at least one command to send
        loop first ([], [])
      where
        loop :: Either AsyncCommand (Command, MVar Result) -> ([Either AsyncCommand Command], [MVar Result]) -> IO (Batch, [MVar Result])
        loop (Left asyncCmd@(SyncWithAnimationFrame _)) (cmds, resultMVars) =
            atomically (readTChan chan) >>= \cmd -> loopAnimation cmd (Left asyncCmd:cmds, resultMVars)
        loop (Right (syncCmd, resultMVar)) (cmds', resultMVars') = do
            let cmds = Right syncCmd:cmds'
                resultMVars = resultMVar:resultMVars'
            atomically (tryReadTChan chan) >>= \case
                Nothing -> return (Batch (reverse cmds) False nBatch, reverse resultMVars)
                Just cmd -> loop cmd (cmds, resultMVars)
        loop (Left asyncCmd) (cmds', resultMVars) = do
            let cmds = Left asyncCmd:cmds'
            atomically (tryReadTChan chan) >>= \case
                Nothing -> return (Batch (reverse cmds) False nBatch, reverse resultMVars)
                Just cmd -> loop cmd (cmds, resultMVars)
        -- When we have seen a SyncWithAnimationFrame command only a synchronous command should end the batch
        loopAnimation :: Either AsyncCommand (Command, MVar Result) -> ([Either AsyncCommand Command], [MVar Result]) -> IO (Batch, [MVar Result])
        loopAnimation (Right (Sync, resultMVar)) (cmds, resultMVars) =
            return (Batch (reverse (Right Sync:cmds)) True nBatch, reverse (resultMVar:resultMVars))
        loopAnimation (Right (syncCmd, resultMVar)) (cmds, resultMVars) =
            atomically (readTChan chan) >>= \cmd -> loopAnimation cmd (Right syncCmd:cmds, resultMVar:resultMVars)
        loopAnimation (Left asyncCmd) (cmds, resultMVars) =
            atomically (readTChan chan) >>= \cmd -> loopAnimation cmd (Left asyncCmd:cmds, resultMVars)

addThreadFinalizer :: ThreadId -> IO () -> IO ()
addThreadFinalizer t@(ThreadId t#) (IO finalizer) =
    IO $ \s -> case mkWeak# t# t finalizer s of { (# s1, _ #) -> (# s1, () #) }

wrapJSVal :: JSValueReceived -> JSM JSVal
wrapJSVal (JSValueReceived ref) = do
    -- TODO make sure this ref has not already been wrapped (perhaps only in debug version)
    let result = JSVal ref
    when (ref >= 5 || ref < 0) $ do
        ctx <- JSM ask
        liftIO . addFinalizer ref $ do
            ft <- takeMVar $ finalizerThreads ctx
            t <- myThreadId
            let tname = T.pack $ show t
            doSendAsyncCommand ctx $ FreeRef tname $ JSValueForSend ref
            if tname `S.member` ft
                then putMVar (finalizerThreads ctx) ft
                else do
                    addThreadFinalizer t $ do
                        modifyMVar (finalizerThreads ctx) $ \s -> return (S.delete tname s, ())
                        doSendAsyncCommand ctx $ FreeRefs tname
                    putMVar (finalizerThreads ctx) =<< evaluate (S.insert tname ft)
    return result
#endif
