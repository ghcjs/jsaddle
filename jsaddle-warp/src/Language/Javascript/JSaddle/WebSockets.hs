{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
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
  -- * Running JSM over WebSockets
    jsaddleOr
  , jsaddleApp
  , jsaddleWithAppOr
  , jsaddleAppPartial
  , debug
  , debugWrapper
) where

import Control.Monad (when, join, void, forever)
import Control.Concurrent (killThread, forkIO, threadDelay)
import Control.Exception (handle, AsyncException, throwIO, fromException, finally)

import Data.Monoid ((<>))
import Data.Aeson (encode, decode)

import Network.Wai
       (Middleware, lazyRequestBody, Application, Request, Response,
        ResponseReceived)
import Network.WebSockets
       (defaultConnectionOptions, ConnectionOptions(..), sendTextData,
        receiveDataMessage, acceptRequest, ServerApp, sendPing)
import qualified Network.WebSockets as WS (DataMessage(..))
import Network.Wai.Handler.WebSockets (websocketsOr)

import Language.Javascript.JSaddle.Types (JSM(..), JSContextRef(..))
import qualified Network.Wai as W
       (responseLBS, requestMethod, pathInfo)
import qualified Data.Text as T (pack)
import qualified Network.HTTP.Types as H
       (status403, status200)
import Language.Javascript.JSaddle.Run (syncPoint, runJavaScript)
import Language.Javascript.JSaddle.Run.Files (indexHtml, runBatch, ghcjsHelpers, initState)
import Language.Javascript.JSaddle.Debug
       (removeContext, addContext)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M (empty, insert, lookup)
import Data.IORef
       (writeIORef, IORef, readIORef, newIORef, atomicModifyIORef')
import Data.ByteString.Lazy (ByteString)
import Control.Concurrent.MVar
       (tryPutMVar, modifyMVar_, putMVar, takeMVar, readMVar, newMVar,
        newEmptyMVar, modifyMVar)
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Foreign.Store (newStore, readStore, lookupStore)
import Language.Javascript.JSaddle (askJSM)
import Control.Monad.IO.Class (MonadIO(..))

jsaddleOr :: ConnectionOptions -> JSM () -> Application -> IO Application
jsaddleOr opts entryPoint otherApp = do
    syncHandlers <- newIORef M.empty
    let wsApp :: ServerApp
        wsApp pending_conn = do
            conn <- acceptRequest pending_conn
            rec (processResult, processSyncResult, start) <- runJavaScript (sendTextData conn . encode) $ do
                    syncKey <- T.pack . show . contextId <$> askJSM
                    liftIO $ atomicModifyIORef' syncHandlers (\m -> (M.insert syncKey processSyncResult m, ()))
                    liftIO $ sendTextData conn (encode syncKey)
                    entryPoint
            _ <- forkIO . forever $
                receiveDataMessage conn >>= \case
                    (WS.Text t) ->
                        case decode t of
                            Nothing -> error $ "jsaddle Results decode failed : " <> show t
                            Just r  -> processResult r
                    _ -> error "jsaddle WebSocket unexpected binary data"
            start
            waitTillClosed conn

        -- Based on Network.WebSocket.forkPingThread
        waitTillClosed conn = ignore `handle` go 1
          where
            go :: Int -> IO ()
            go i = do
                threadDelay (1 * 1000 * 1000)
                sendPing conn (T.pack $ show i)
                go (i + 1)

        ignore e = case fromException e of
            Just async -> throwIO (async :: AsyncException)
            Nothing    -> return ()

        syncHandler :: Application
        syncHandler req sendResponse = case (W.requestMethod req, W.pathInfo req) of
            ("POST", ["sync", syncKey]) -> do
                body <- lazyRequestBody req
                case decode body of
                    Nothing -> error $ "jsaddle sync message decode failed : " <> show body
                    Just result ->
                        M.lookup syncKey <$> readIORef syncHandlers >>= \case
                            Nothing -> error "jsaddle missing sync message handler"
                            Just handler -> do
                                next <- encode <$> handler result
                                sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/json")] next
            _ -> otherApp req sendResponse
    return $ websocketsOr opts wsApp syncHandler


jsaddleApp :: Application
jsaddleApp req sendResponse =
    fromMaybe
        (sendResponse $  W.responseLBS H.status403 [("Content-Type", "text/plain")] "Forbidden")
        (jsaddleAppPartial req sendResponse)

jsaddleWithAppOr :: ConnectionOptions -> JSM () -> Application -> IO Application
jsaddleWithAppOr opts entryPoint otherApp = jsaddleOr opts entryPoint $ \req sendResponse ->
  (fromMaybe (otherApp req sendResponse)
     (jsaddleAppPartial req sendResponse))

jsaddleAppPartial :: Request -> (Response -> IO ResponseReceived) -> Maybe (IO ResponseReceived)
jsaddleAppPartial req sendResponse = case (W.requestMethod req, W.pathInfo req) of
    ("GET", []) -> Just $ sendResponse $ W.responseLBS H.status200 [("Content-Type", "text/html")] indexHtml
    ("GET", ["jsaddle.js"]) -> Just $ sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/javascript")] jsaddleJs
    _ -> Nothing

-- Use this to generate this string for embedding
-- sed -e 's|\\|\\\\|g' -e 's|^|    \\|' -e 's|$|\\n\\|' -e 's|"|\\"|g' data/jsaddle.js | pbcopy
jsaddleJs :: ByteString
jsaddleJs = "\
    \if(typeof global !== \"undefined\") {\n\
    \    global.window = global;\n\
    \    global.WebSocket = require('ws');\n\
    \}\n\
    \\n\
    \var connect = function() {\n\
    \    var wsaddress = window.location.protocol.replace('http', 'ws')+\"//\"+window.location.hostname+(window.location.port?(\":\"+window.location.port):\"\");\n\
    \\n\
    \    var ws = new WebSocket(wsaddress);\n\
    \    var syncKey = \"\";\n\
    \\n\
    \    ws.onopen = function(e) {\n\
    \ " <> initState <> "\n\
    \\n\
    \        ws.onmessage = function(e) {\n\
    \            var batch = JSON.parse(e.data);\n\
    \            if(inCallback > 0) {\n\
    \                asyncBatch = batch;\n\
    \                return;\n\
    \            }\n\
    \            if(typeof batch === \"string\") {\n\
    \                syncKey = batch;\n\
    \                var xhr = new XMLHttpRequest();\n\
    \                xhr.open('POST', '/reload/'+syncKey, true);\n\
    \                xhr.onreadystatechange = function() {\n\
    \                    if(xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200)\n\
    \                        setTimeout(function(){window.location.reload();}, 500);\n\
    \                };\n\
    \                xhr.send();\n\
    \                return;\n\
    \            }\n\
    \\n\
    \ " <> runBatch (\a -> "ws.send(JSON.stringify(" <> a <> "));")
              (Just (\a -> "(function(){\n\
                  \                       var xhr = new XMLHttpRequest();\n\
                  \                       xhr.open('POST', '/sync/'+syncKey, false);\n\
                  \                       xhr.setRequestHeader(\"Content-type\", \"application/json\");\n\
                  \                       xhr.send(JSON.stringify(" <> a <> "));\n\
                  \                       return JSON.parse(xhr.response);})()")) <> "\
    \        };\n\
    \    };\n\
    \    ws.onerror = function() {\n\
    \        setTimeout(connect, 1000);\n\
    \    };\n\
    \}\n\
    \\n\
    \ " <> ghcjsHelpers <> "\
    \connect();\n\
    \"

-- | Start or restart the server.
-- To run this as part of every :reload use
-- > :def! reload (const $ return "::reload\nLanguage.Javascript.JSaddle.Warp.debug 3708 SomeMainModule.someMainFunction")
debug :: Int -> JSM () -> IO ()
debug port f = do
    debugWrapper $ \refreshMiddleware registerContext ->
        runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
            jsaddleOr defaultConnectionOptions (registerContext >> f >> syncPoint) (refreshMiddleware jsaddleApp)
    putStrLn $ "<a href=\"http://localhost:" <> show port <> "\">run</a>"

debugWrapper :: (Middleware -> JSM () -> IO ()) -> IO ()
debugWrapper run = do
    reloadMVar <- newEmptyMVar
    reloadDoneMVars <- newMVar []
    contexts <- newMVar []
    let refreshMiddleware :: Middleware
        refreshMiddleware otherApp req sendResponse = case (W.requestMethod req, W.pathInfo req) of
            ("POST", ["reload", _syncKey]) -> do
                reloadDone <- newEmptyMVar
                modifyMVar_ reloadDoneMVars (return . (reloadDone:))
                readMVar reloadMVar
                r <- sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/json")] ("reload" :: ByteString)
                putMVar reloadDone ()
                return r
            _ -> otherApp req sendResponse
        start :: Int -> IO (IO Int)
        start expectedConnections = do
            serverDone <- newEmptyMVar
            ready <- newEmptyMVar
            let registerContext :: JSM ()
                registerContext = do
                    uuid <- contextId <$> askJSM
                    browsersConnected <- liftIO $ modifyMVar contexts (\ctxs -> return (uuid:ctxs, length ctxs + 1))
                    addContext
                    when (browsersConnected == expectedConnections) . void . liftIO $ tryPutMVar ready ()
            thread <- forkIO $
                finally (run refreshMiddleware registerContext)
                    (putMVar serverDone ())
            _ <- forkIO $ threadDelay 10000000 >> void (tryPutMVar ready ())
            when (expectedConnections /= 0) $ takeMVar ready
            return $ do
                putMVar reloadMVar ()
                ctxs <- takeMVar contexts
                mapM_ removeContext ctxs
                takeMVar reloadDoneMVars >>= mapM_ takeMVar
                killThread thread
                takeMVar serverDone
                return $ length ctxs
    lookupStore shutdown_0 >>= \case
        Nothing -> do
            shutdownRef <- newIORef =<< start 0
            void $ newStore shutdownRef
        Just shutdownStore -> do
            shutdownRef :: IORef (IO Int) <- readStore shutdownStore
            expectedConnections <- join (readIORef shutdownRef)
            writeIORef shutdownRef =<< start expectedConnections
  where shutdown_0 = 0
