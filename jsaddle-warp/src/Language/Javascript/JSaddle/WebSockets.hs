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
  , jsaddleAppPartial
  , jsaddleJs
) where

import Control.Monad (forever, join)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (handle, AsyncException, throwIO, fromException)

import Data.Monoid ((<>))
import Data.Aeson (encode, decode)

import Network.Wai
       (lazyRequestBody, Application, Request, Response,
        ResponseReceived)
import Network.WebSockets
       (ConnectionOptions(..), sendTextData,
        receiveDataMessage, acceptRequest, ServerApp, sendPing)
import qualified Network.WebSockets as WS (DataMessage(..))
import Network.Wai.Handler.WebSockets (websocketsOr)

import Language.Javascript.JSaddle.Types (JSM(..))
import qualified Network.Wai as W
       (responseLBS, requestMethod, pathInfo)
import qualified Data.Text as T (pack)
import qualified Network.HTTP.Types as H
       (status403, status200)
import Language.Javascript.JSaddle.Run (runJS)
import Language.Javascript.JSaddle.Run.Files (indexHtml, jsaddleCoreJs, ghcjsHelpers)
import Data.Maybe (fromMaybe)
import Data.IORef
       (readIORef, newIORef, writeIORef)
import Data.ByteString.Lazy (ByteString)
import Language.Javascript.JSaddle (SyncCallbackId, ValId)
import Control.Monad.Trans.Reader

jsaddleOr :: ConnectionOptions -> JSM () -> Application -> IO Application
jsaddleOr opts entryPoint otherApp = do
    --TODO: Allow multiple simultaneous connections
    processSyncResultRef <- newIORef $ error "processSyncResult not yet set up"
    let processSyncResult callback this args = do
          f <- readIORef processSyncResultRef
          f callback this args
    continueSyncCallbackRef <- newIORef $ error "continueSyncCallback not yet set up"
    let continueSyncCallback = do
          join $ readIORef continueSyncCallbackRef
    let wsApp :: ServerApp
        wsApp pending_conn = do
            conn <- acceptRequest pending_conn
            (processResult, processSyncResult', continueSyncCallback', env) <- runJS (sendTextData conn . encode)
            writeIORef processSyncResultRef processSyncResult'
            writeIORef continueSyncCallbackRef continueSyncCallback'
            _ <- forkIO . forever $
                receiveDataMessage conn >>= \case
                    (WS.Text t) ->
                        case decode t of
                            Nothing -> error $ "jsaddle Results decode failed : " <> show t
                            Just r  -> processResult r
                    _ -> error "jsaddle WebSocket unexpected binary data"
            runReaderT (unJSM entryPoint) env
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
            ("POST", ["sync"]) -> do
                body <- lazyRequestBody req
                case decode body :: Maybe (Maybe (SyncCallbackId, ValId, [ValId])) of
                    Nothing -> error $ "jsaddle sync message decode failed : " <> show body
                    Just parsed -> do
                      --TODO: Combine processSyncResult with continueSyncCallback
                      result <- case parsed of
                        Just (callback, this, args) -> processSyncResult callback this args
                        Nothing -> continueSyncCallback
                      sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/json")] $ encode result
            _ -> otherApp req sendResponse
    return $ websocketsOr opts wsApp syncHandler


jsaddleApp :: Application
jsaddleApp = jsaddleAppWithJs $ jsaddleJs False

jsaddleAppWithJs :: ByteString -> Application
jsaddleAppWithJs js req sendResponse =
    fromMaybe
        (sendResponse $  W.responseLBS H.status403 [("Content-Type", "text/plain")] "Forbidden")
        (jsaddleAppPartialWithJs js req sendResponse)

jsaddleWithAppOr :: ConnectionOptions -> JSM () -> Application -> IO Application
jsaddleWithAppOr opts entryPoint otherApp = jsaddleOr opts entryPoint $ \req sendResponse ->
  (fromMaybe (otherApp req sendResponse)
     (jsaddleAppPartial req sendResponse))

jsaddleAppPartial :: Request -> (Response -> IO ResponseReceived) -> Maybe (IO ResponseReceived)
jsaddleAppPartial = jsaddleAppPartialWithJs $ jsaddleJs False

jsaddleAppPartialWithJs :: ByteString -> Request -> (Response -> IO ResponseReceived) -> Maybe (IO ResponseReceived)
jsaddleAppPartialWithJs js req sendResponse = case (W.requestMethod req, W.pathInfo req) of
    ("GET", []) -> Just $ sendResponse $ W.responseLBS H.status200 [("Content-Type", "text/html")] indexHtml
    ("GET", ["jsaddle.js"]) -> Just $ sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/javascript")] js
    _ -> Nothing

-- Use this to generate this string for embedding
-- sed -e 's|\\|\\\\|g' -e 's|^|    \\|' -e 's|$|\\n\\|' -e 's|"|\\"|g' data/jsaddle.js | pbcopy
jsaddleJs :: Bool -> ByteString
jsaddleJs refreshOnLoad = jsaddleCoreJs <> "\
    \if(typeof global !== \"undefined\") {\n\
    \    global.window = global;\n\
    \    global.WebSocket = require('ws');\n\
    \}\n\
    \\n\
    \var connect = function() {\n\
    \    var wsaddress = window.location.protocol.replace('http', 'ws')+\"//\"+window.location.hostname+(window.location.port?(\":\"+window.location.port):\"\");\n\
    \\n\
    \    var ws = new WebSocket(wsaddress);\n\
    \    var sync = function(v) {\n\
    \      var xhr = new XMLHttpRequest();\n\
    \      xhr.open('POST', '/sync', false);\n\
    \      xhr.setRequestHeader(\"Content-type\", \"application/json\");\n\
    \      xhr.send(JSON.stringify(v));\n\
    \      return JSON.parse(xhr.response);\n\
    \    };\n\
    \    var core = jsaddle(window, function(a) {\n\
    \      ws.send(JSON.stringify(a));\n\
    \    }, function(callback, that, args) {\n\
    \      return sync([callback, that, args]);\n\
    \    }, function() {\n\
    \      return sync(null);\n\
    \    });\n\
    \    var syncKey = \"\";\n\
    \\n\
    \    ws.onopen = function(e) {\n\
    \\n\
    \        ws.onmessage = function(e) {\n\
    \          core.processReq(JSON.parse(e.data));\n\
    \        }\n\
    \    };\n\
    \    ws.onerror = function() {\n\
    \        setTimeout(connect, 1000);\n\
    \    };\n\
    \}\n\
    \\n\
    \ " <> ghcjsHelpers <> "\
    \connect();\n\
    \"
