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
  , jsaddleAppWithJs
  , jsaddleAppWithJsOr
  , jsaddleAppPartial
  , jsaddleJs
  , jsaddleJs'
  , debug
  , debugOr
  , debugWrapper
) where

import Control.Monad (when, void, forever)
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
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.HTTP.Types (Status(..))

import Language.Javascript.JSaddle.Types (JSM(..), JSContextRef(..))
import qualified Network.Wai as W
       (responseLBS, requestMethod, pathInfo, modifyResponse, responseStatus)
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
       (readIORef, newIORef, atomicModifyIORef')
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
       (toStrict, stripPrefix)
import Control.Concurrent.MVar
       (tryTakeMVar, MVar, tryPutMVar, modifyMVar_, putMVar, takeMVar,
        readMVar, newMVar, newEmptyMVar, modifyMVar)
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Foreign.Store (readStore, lookupStore, writeStore, Store(..))
import Language.Javascript.JSaddle (askJSM)
import Control.Monad.IO.Class (MonadIO(..))

import Language.Javascript.JSaddle.WebSockets.Compat (getTextMessageByteString)
import qualified Data.Text.Encoding as T (decodeUtf8)

jsaddleOr :: ConnectionOptions -> JSM () -> Application -> IO Application
jsaddleOr opts entryPoint otherApp = do
    syncHandlers <- newIORef M.empty
    asyncHandlers <- newIORef M.empty
    let wsApp :: ServerApp
        wsApp pending_conn = do
            conn <- acceptRequest pending_conn
            initMsg <- receiveDataMessage conn
            case getTextMessageByteString initMsg of
                Just "" -> do
                    rec (processResult, processSyncResult, start) <- runJavaScript (sendTextData conn . encode) $ do
                            syncKey <- T.pack . show . contextId <$> askJSM
                            liftIO $ atomicModifyIORef' syncHandlers (\m -> (M.insert syncKey processSyncResult m, ()))
                            liftIO $ atomicModifyIORef' asyncHandlers (\m -> (M.insert syncKey processResult m, ()))
                            liftIO $ sendTextData conn (encode syncKey)
                            entryPoint
                    start
                    waitTillClosed conn
                Just syncKey ->
                    M.lookup (T.decodeUtf8 $ LBS.toStrict syncKey) <$> readIORef syncHandlers >>= \case
                      Nothing -> error "jsaddle missing sync message handler"
                      Just processResult ->
                        forever $
                          receiveDataMessage conn >>= \msg -> case getTextMessageByteString msg of
                              Just t ->
                                  case decode t of
                                      Nothing -> error $ "jsaddle Results decode failed : " <> show t
                                      Just r  -> processResult r
                              _ -> error "jsaddle WebSocket unexpected binary data"
                _ -> error "jsaddle WebSocket unexpected binary data"

        -- Based on Network.WebSocket.forkPingThread
        waitTillClosed conn = ignore `handle` go 1
          where
            go :: Int -> IO ()
            go i = do
                threadDelay (10 * 1000 * 1000)
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
            (method, _) -> (catch404 otherApp) req sendResponse
              where catch404 = W.modifyResponse $ \resp ->
                      case (method, W.responseStatus resp) of
                        ("GET", Status 404 _) -> indexResponse
                        _ -> resp
    return $ websocketsOr opts wsApp syncHandler


jsaddleApp :: Application
jsaddleApp = jsaddleAppWithJs $ jsaddleJs False

jsaddleAppWithJs :: ByteString -> Application
jsaddleAppWithJs js req sendResponse =
  jsaddleAppWithJsOr js
    (\_ _ -> sendResponse $ W.responseLBS H.status403 [("Content-Type", "text/plain")] "Forbidden")
    req sendResponse

jsaddleAppWithJsOr :: ByteString -> Application -> Application
jsaddleAppWithJsOr js otherApp req sendResponse =
  fromMaybe (otherApp req sendResponse)
    (jsaddleAppPartialWithJs js req sendResponse)

jsaddleWithAppOr :: ConnectionOptions -> JSM () -> Application -> IO Application
jsaddleWithAppOr opts entryPoint otherApp = jsaddleOr opts entryPoint $ \req sendResponse ->
  (fromMaybe (otherApp req sendResponse)
     (jsaddleAppPartial req sendResponse))

jsaddleAppPartial :: Request -> (Response -> IO ResponseReceived) -> Maybe (IO ResponseReceived)
jsaddleAppPartial = jsaddleAppPartialWithJs $ jsaddleJs False

indexResponse :: Response
indexResponse = W.responseLBS H.status200 [("Content-Type", "text/html")] indexHtml

jsaddleAppPartialWithJs :: ByteString -> Request -> (Response -> IO ResponseReceived) -> Maybe (IO ResponseReceived)
jsaddleAppPartialWithJs js req sendResponse = case (W.requestMethod req, W.pathInfo req) of
    ("GET", []) -> Just $ sendResponse indexResponse
    ("GET", ["jsaddle.js"]) -> Just $ sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/javascript")] js
    _ -> Nothing

jsaddleJs :: Bool -> ByteString
jsaddleJs = jsaddleJs' Nothing

-- Use this to generate this string for embedding
-- sed -e 's|\\|\\\\|g' -e 's|^|    \\|' -e 's|$|\\n\\|' -e 's|"|\\"|g' data/jsaddle.js | pbcopy
jsaddleJs' :: Maybe ByteString -> Bool -> ByteString
jsaddleJs' jsaddleUri refreshOnLoad = "\
    \if(typeof global !== \"undefined\" && typeof require === \"function\") {\n\
    \    global.window = global;\n\
    \    global.WebSocket = require('ws');\n\
    \}\n\
    \\n\
    \var connect = function() {\n\
    \    var wsaddress = "
      <> maybe "window.location.protocol.replace('http', 'ws')+\"//\"+window.location.hostname+(window.location.port?(\":\"+window.location.port):\"\")"
            (\ s -> "\"ws" <> s <> "\"")
            (jsaddleUri >>= LBS.stripPrefix "http")
      <> ";\n\
    \\n\
    \    var ws0 = new WebSocket(wsaddress);\n\
    \    var syncKey = \"\";\n\
    \\n\
    \    ws0.onopen = function(e) {\n\
    \        ws0.send(\"\");\n\
    \        var initialResults = [];\n\
    \        var ws = {send: function(m) {initialResults.push(m);}};\n\
    \ " <> initState <> "\n\
    \\n\
    \        ws0.onmessage = function(e) {\n\
    \            var batch = JSON.parse(e.data);\n\
    \            if(inCallback > 0) {\n\
    \                asyncBatch = batch;\n\
    \                return;\n\
    \            }\n\
    \            if(typeof batch === \"string\") {\n\
    \                syncKey = batch;\n\
    \                var ws1 = new WebSocket(wsaddress);\n\
    \                ws1.onopen = function(e) {\n\
    \                    ws1.send(syncKey);\n\
    \                    initialResults.forEach(function(m){ ws1.send(m); });\n\
    \                    initialResults = null;\n\
    \                    ws = ws1;\n\
    \                }\n" <>
    (if refreshOnLoad
     then "                var xhr = new XMLHttpRequest();\n\
          \                xhr.open('POST', '/reload/'+syncKey, true);\n\
          \                xhr.onreadystatechange = function() {\n\
          \                    if(xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200)\n\
          \                        setTimeout(function(){window.location.reload();}, 100);\n\
          \                };\n\
          \                xhr.send();\n"
     else "") <>
    "                return;\n\
    \            }\n\
    \\n\
    \ " <> runBatch (\a -> "ws.send(JSON.stringify(" <> a <> "));")
              (Just (\a -> "(function(){\n\
                  \                       var xhr = new XMLHttpRequest();\n\
                  \                       xhr.open('POST', '" <> fromMaybe "" jsaddleUri <> "/sync/'+syncKey, false);\n\
                  \                       xhr.setRequestHeader(\"Content-type\", \"application/json\");\n\
                  \                       xhr.send(JSON.stringify(" <> a <> "));\n\
                  \                       return JSON.parse(xhr.response);})()")) <> "\
    \        };\n\
    \    };\n\
    \    ws0.onerror = function() {\n\
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
    debugWrapper $ \withRefresh registerContext ->
        runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
            jsaddleOr defaultConnectionOptions (registerContext >> f >> syncPoint) (withRefresh $ jsaddleAppWithJs $ jsaddleJs True)
    putStrLn $ "<a href=\"http://localhost:" <> show port <> "\">run</a>"

debugOr :: Int -> JSM () -> Application -> IO ()
debugOr port f b = do
    debugWrapper $ \withRefresh registerContext ->
        runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
            jsaddleOr defaultConnectionOptions (registerContext >> f >> syncPoint) (withRefresh $ jsaddleAppWithJsOr (jsaddleJs True) b)
    putStrLn $ "<a href=\"http://localhost:" <> show port <> "\">run</a>"

refreshMiddleware :: ((Response -> IO ResponseReceived) -> IO ResponseReceived) -> Middleware
refreshMiddleware refresh otherApp req sendResponse = case (W.requestMethod req, W.pathInfo req) of
    ("POST", ["reload", _syncKey]) -> refresh sendResponse
    _ -> otherApp req sendResponse

debugWrapper :: (Middleware -> JSM () -> IO ()) -> IO ()
debugWrapper run = do
    reloadMVar <- newEmptyMVar
    reloadDoneMVars <- newMVar []
    contexts <- newMVar []
    let refresh sendResponse = do
          reloadDone <- newEmptyMVar
          modifyMVar_ reloadDoneMVars (return . (reloadDone:))
          readMVar reloadMVar
          r <- sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/json")] ("reload" :: ByteString)
          putMVar reloadDone ()
          return r
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
                finally (run (refreshMiddleware refresh) registerContext)
                    (putMVar serverDone ())
            _ <- forkIO $ threadDelay 10000000 >> void (tryPutMVar ready ())
            when (expectedConnections /= 0) $ takeMVar ready
            return $ do
                putMVar reloadMVar ()
                ctxs <- takeMVar contexts
                mapM_ removeContext ctxs
                takeMVar reloadDoneMVars >>= mapM_ takeMVar
                tryTakeMVar serverDone >>= \case
                    Nothing -> do
                        killThread thread
                        takeMVar serverDone
                    Just _ -> return ()
                return $ length ctxs
        restarter :: MVar (Int -> IO (IO Int)) -> IO Int -> IO ()
        restarter mvar stop = do
             start' <- takeMVar mvar
             n <- stop
             start' n >>= restarter mvar
    lookupStore storeId >>= \case
        Nothing -> do
            restartMVar <- newMVar start
            void . forkIO $ restarter restartMVar (return 0)
            void $ writeStore (Store storeId) restartMVar
        Just shutdownStore -> do
            restartMVar :: MVar (Int -> IO (IO Int)) <- readStore shutdownStore
            void $ tryTakeMVar restartMVar
            putMVar restartMVar start
  where storeId = 354
