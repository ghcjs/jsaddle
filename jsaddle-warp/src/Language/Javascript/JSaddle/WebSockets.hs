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
  -- * Running JSM over WebSockets
    jsaddleOr
  , jsaddleApp
  , jsaddleWithAppOr
  , jsaddleAppPartial
) where

import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (handle, AsyncException, throwIO, fromException)

import Data.Monoid ((<>))
import Data.Aeson (encode, decode)

import Network.Wai
       (requestBody, Application, Request, Response, ResponseReceived)
import Network.WebSockets
       (ConnectionOptions(..), sendTextData, receiveDataMessage,
        acceptRequest, ServerApp, sendPing)
import qualified Network.WebSockets as WS (DataMessage(..))
import Network.Wai.Handler.WebSockets (websocketsOr)

import Language.Javascript.JSaddle.Types (JSM(..))
import qualified Network.Wai as W
       (responseLBS, requestMethod, pathInfo)
import qualified Data.Text as T (pack)
import qualified Network.HTTP.Types as H
       (status403, status200)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (indexHtml, runBatch, ghcjsHelpers, initState)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M (empty, insert, lookup)
import Data.IORef (readIORef, newIORef, atomicModifyIORef')
import Data.ByteString.Lazy (fromStrict, ByteString)
import Data.Text (Text)
import Data.UUID.Types (toText)
import Data.UUID.V4 (nextRandom)

jsaddleOr :: ConnectionOptions -> JSM () -> Application -> IO Application
jsaddleOr opts entryPoint otherApp = do
    syncHandlers <- newIORef M.empty
    let wsApp :: ServerApp
        wsApp pending_conn = do
            conn <- acceptRequest pending_conn
            (processResult, processSyncResult, start) <- runJavaScript (sendTextData conn . encode) entryPoint
            _ <- forkIO . forever $
                receiveDataMessage conn >>= \case
                    (WS.Text t) ->
                        case decode t of
                            Nothing -> error $ "jsaddle Results decode failed : " <> show t
                            Just r  -> processResult r
                    _ -> error "jsaddle WebSocket unexpected binary data"
            syncKey <- toText <$> nextRandom
            atomicModifyIORef' syncHandlers (\m -> (M.insert syncKey processSyncResult m, ()))
            sendTextData conn (encode syncKey)
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
                body <- requestBody req
                case decode $ fromStrict body of
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
    \        var inXhrSend = false;\n\
    \\n\
    \        ws.onmessage = function(e) {\n\
    \            if(inXhrSend) return;\n\
    \            var batch = JSON.parse(e.data);\n\
    \            if(typeof batch === \"string\") {\n\
    \                syncKey = batch;\n\
    \                return;\n\
    \            }\n\
    \\n\
    \ " <> runBatch (\a -> "ws.send(JSON.stringify(" <> a <> "));")
              (Just (\a -> "(function(){\n\
                  \                       inXhrSend = true;\n\
                  \                       var xhr = new XMLHttpRequest();\n\
                  \                       xhr.open('POST', '/sync/'+syncKey, false);\n\
                  \                       xhr.setRequestHeader(\"Content-type\", \"application/json\");\n\
                  \                       xhr.send(JSON.stringify(" <> a <> "));\n\
                  \                       inXhrSend = false;\n\
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

