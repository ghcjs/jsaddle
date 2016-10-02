{-# LANGUAGE TemplateHaskell #-}
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

module Language.Javascript.JSaddle.WebSockets (
    jsaddleOr
  , run
  , AsyncCommand(..)
  , Command(..)
  , Result(..)
  , sendCommand
  , sendLazyCommand
  , sendAsyncCommand
  , syncPoint
  , syncAfter
) where

import Control.Exception (throwIO)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (asks)
import Control.Monad.STM (STM, atomically)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
       (tryReadTChan, TChan, readTChan, writeTChan, newTChanIO)
import Control.Concurrent.STM.TVar
       (writeTVar, readTVar, readTVarIO, modifyTVar, newTVarIO)
import Control.Concurrent.MVar
       (MVar, MVar, putMVar, takeMVar, newEmptyMVar)

import Data.Monoid ((<>))
import qualified Data.Text as T (unpack)
import qualified Data.Map as M (lookup, delete, insert, empty)
import Data.Aeson (encode, decode)

import Network.Wai (Application)
import Network.WebSockets
       (ConnectionOptions(..), sendTextData, receiveDataMessage,
        acceptRequest, defaultConnectionOptions, ServerApp)
import qualified Network.WebSockets as WS (DataMessage(..))
import Network.Wai.Handler.WebSockets (websocketsOr)

import Language.Javascript.JSaddle.Types
       (Command(..), AsyncCommand(..), Result(..), JSContextRef(..), JSVal(..),
        Object(..), JSValueReceived(..), JSM, Batch(..), JSValueForSend(..), runJSaddle)
import Language.Javascript.JSaddle.Exception (JSException(..))
import Language.Javascript.JSaddle.Native (wrapJSVal)
import Language.Javascript.JSaddle.WebSockets.Files (mkEmbedded)
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.Wai.Application.Static
       (ssIndices, staticApp)
import WaiAppStatic.Storage.Embedded (mkSettings)
import WaiAppStatic.Types (unsafeToPiece)

sendCommand :: Command -> JSM Result
sendCommand cmd = do
    s <- asks doSendCommand
    liftIO $ s cmd

sendLazyCommand :: (JSValueForSend -> AsyncCommand) -> JSM JSVal
sendLazyCommand cmd = do
    nextRefTVar <- asks nextRef
    n <- liftIO . atomically $ do
        n <- subtract 1 <$> readTVar nextRefTVar
        writeTVar nextRefTVar n
        return n
    s <- asks doSendAsyncCommand
    liftIO $ s (cmd $ JSValueForSend n)
    wrapJSVal (JSValueReceived n)

sendAsyncCommand :: AsyncCommand -> JSM ()
sendAsyncCommand cmd = do
    s <- asks doSendAsyncCommand
    liftIO $ s cmd

-- | Forces execution of pending asyncronous code 
syncPoint :: JSM ()
syncPoint = void $ sendCommand Sync

-- | Forces execution of pending asyncronous code after performing `f` 
syncAfter :: JSM a -> JSM a
syncAfter f = do
    result <- f
    syncPoint
    return result

jsaddleOr :: ConnectionOptions -> JSM () -> Application -> Application
jsaddleOr opts entryPoint = websocketsOr opts wsApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
        conn <- acceptRequest pending_conn
        recvChan <- newTChanIO
        commandChan <- newTChanIO
        callbacks <- newTVarIO M.empty
        nextRef <- newTVarIO 0
        let ctx = JSContextRef {
            doSendCommand = \cmd -> do
                result <- newEmptyMVar
                atomically $ writeTChan commandChan (Right (cmd, result))
                takeMVar result >>= \case
                    (ThrowJSValue (JSValueReceived v)) -> throwIO $ JSException (JSVal v)
                    result -> return result
          , doSendAsyncCommand = atomically . writeTChan commandChan . Left
          , addCallback = \(Object (JSVal val)) cb -> atomically $ modifyTVar callbacks (M.insert val cb)
          , freeCallback = \(Object (JSVal val)) -> atomically $ modifyTVar callbacks (M.delete val)
          , nextRef = nextRef
          }
        forkIO . forever $
            receiveDataMessage conn >>= \case
                (WS.Text t) ->
                    case decode t of
                        Nothing                  -> error $ "jsaddle WebSocket decode failed : " <> show t
                        Just (ProtocolError err) -> error $ "Protocol error : " <> T.unpack err
                        Just (Callback f this a) -> do
                            f'@(JSVal fNumber) <- runJSaddle ctx $ wrapJSVal f
                            this' <- runJSaddle ctx $ wrapJSVal this
                            args <- runJSaddle ctx $ mapM wrapJSVal a
                            (M.lookup fNumber <$> liftIO (readTVarIO callbacks)) >>= \case
                                Nothing -> liftIO $ putStrLn "Callback called after it was freed"
                                Just cb -> void . forkIO . runJSaddle ctx $ cb f' this' args
                        Just m                   -> atomically $ writeTChan recvChan m
                _ -> error "jsaddle WebSocket unexpected binary data"
        forkIO . forever $ atomically (readBatch commandChan) >>= \case
                (batch, Just resultMVar) -> do
                    sendTextData conn $ encode batch
                    atomically (readTChan recvChan) >>= putMVar resultMVar
                (batch, Nothing) -> do
                    sendTextData conn $ encode batch
                    atomically (readTChan recvChan) >>= \case
                        SyncResult -> return ()
                        ThrowJSValue e -> atomically (discardToSyncPoint commandChan) >>= (`putMVar` ThrowJSValue e)
                        _ -> error "Unexpected result processing batch"
                    return ()
        runJSaddle ctx entryPoint

    readBatch :: TChan (Either AsyncCommand (Command, MVar Result)) -> STM (Batch, Maybe (MVar Result))
    readBatch chan = do
        first <- readTChan chan -- We want at least one command to send
        loop first []
      where
        loop (Right (cmd, resultMVar)) asyncCmds =
            return (Batch (reverse asyncCmds) cmd, Just resultMVar)
        loop (Left asyncCmd) asyncCmds' = do
            let asyncCmds = asyncCmd:asyncCmds'
            tryReadTChan chan >>= \case
                Nothing -> return (Batch (reverse asyncCmds) Sync, Nothing)
                Just cmd -> loop cmd asyncCmds
    discardToSyncPoint :: TChan (Either AsyncCommand (Command, MVar Result)) -> STM (MVar Result)
    discardToSyncPoint chan =
        readTChan chan >>= \case
            Right (_, resultMVar) -> return resultMVar
            _                     -> discardToSyncPoint chan

jsaddleApp :: Application
jsaddleApp = staticApp ($(mkSettings mkEmbedded)) {ssIndices = [unsafeToPiece "index.html"]}

run :: Int -> JSM () -> IO ()
run port f =
    runSettings (setPort port (setTimeout 3600 defaultSettings)) $
        jsaddleOr defaultConnectionOptions (f >> syncPoint) jsaddleApp
