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

import Control.Monad (forever)
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
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.HTTP.Types (Status(..))

import Language.Javascript.JSaddle.Types (JSM(..))
import qualified Network.Wai as W
       (responseLBS, requestMethod, pathInfo, modifyResponse, responseStatus)
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.Text as T (pack)
import Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP.Types as H
       (status403, status200)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (indexHtml, jsaddleCoreJs, ghcjsHelpers)
import Data.Maybe (fromMaybe)
import Data.IORef
       (readIORef, newIORef, atomicModifyIORef')
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS (stripPrefix)
import Language.Javascript.JSaddle (runJSM)
import qualified Data.Map as Map
import System.Entropy (getEntropy)
import Control.Exception (try, SomeException (..))

--TODO: stylish-haskell

import Language.Javascript.JSaddle.WebSockets.Compat (getTextMessageByteString)

jsaddleOr :: ConnectionOptions -> JSM () -> Application -> IO Application
jsaddleOr opts entryPoint otherApp = do
    syncFuncs <- newIORef Map.empty
    let wsApp :: ServerApp
        wsApp pending_conn = do
            conn <- acceptRequest pending_conn
            (processResult, processSyncCommand, env) <- runJavaScript $ \req -> do
              sendTextData conn $ encode req
            connId <- decodeUtf8 . Base64URL.encode <$> getEntropy 24
            sendTextData conn connId
            atomicModifyIORef' syncFuncs $ \fs ->
              ( Map.insertWith (error $ "duplicate connection ID" <> show connId) connId processSyncCommand fs
              , ()
              )
            _ <- forkIO . forever $
                receiveDataMessage conn >>= \msg -> case getTextMessageByteString msg of
                    Just t -> case decode t of
                        Nothing -> putStrLn $ "jsaddle response decode failed: " <> show t
                        Just r  -> do
                          result <- try $ processResult r
                          case result of
                            Left e@(SomeException _) -> putStrLn $ "jsaddle processResult failed: " <> show e
                            Right _ -> return ()
                    _ -> error "jsaddle WebSocket unexpected binary data"
            try (runJSM entryPoint env) >>= \case
              Left e@(SomeException _) -> putStrLn $ "done: left: " <> show e
              Right _ -> putStrLn $ "done: right"
            waitTillClosed conn
            atomicModifyIORef' syncFuncs $ \fs -> (Map.delete connId fs, ())

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
            ("POST", ["sync", connId]) -> do
                Just syncFunc <- Map.lookup connId <$> readIORef syncFuncs
                body <- lazyRequestBody req
                case decode body of
                    Nothing -> error $ "jsaddle sync message decode failed: " <> show body
                    Just parsed -> do
                      result <- syncFunc parsed
                      sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/json")] $ encode result
            (method, _) -> (catch404 otherApp) req sendResponse
              where catch404 = W.modifyResponse $ \resp ->
                      case (method, W.responseStatus resp) of
                        ("GET", Status 404 _) -> indexResponse
                        _ -> resp
    return $ websocketsOr opts wsApp syncHandler


jsaddleApp :: Application
jsaddleApp = jsaddleAppWithJs $ jsaddleJs Nothing False

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
jsaddleAppPartial = jsaddleAppPartialWithJs $ jsaddleJs Nothing False

indexResponse :: Response
indexResponse = W.responseLBS H.status200 [("Content-Type", "text/html")] indexHtml

jsaddleAppPartialWithJs :: ByteString -> Request -> (Response -> IO ResponseReceived) -> Maybe (IO ResponseReceived)
jsaddleAppPartialWithJs js req sendResponse = case (W.requestMethod req, W.pathInfo req) of
    ("GET", []) -> Just $ sendResponse indexResponse
    ("GET", ["jsaddle.js"]) -> Just $ sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/javascript")] js
    _ -> Nothing

--TODO: Make refreshOnLoad work
-- Use this to generate this string for embedding
-- sed -e 's|\\|\\\\|g' -e 's|^|    \\|' -e 's|$|\\n\\|' -e 's|"|\\"|g' data/jsaddle.js | pbcopy
jsaddleJs :: Maybe ByteString -> Bool -> ByteString
jsaddleJs jsaddleUri refreshOnLoad = jsaddleCoreJs <> "\
    \if(typeof global !== \"undefined\") {\n\
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
    \    var ws = new WebSocket(wsaddress);\n\
    \    var connId;\n\
    \    var sync = function(v) {\n\
    \      var xhr = new XMLHttpRequest();\n\
    \      xhr.open('POST', '" <> fromMaybe "" jsaddleUri <> "/sync/' + connId, false);\n\
    \      xhr.setRequestHeader(\"Content-type\", \"application/json\");\n\
    \      xhr.send(JSON.stringify(v));\n\
    \      return JSON.parse(xhr.response);\n\
    \    };\n\
    \    var core = jsaddle(window, function(a) {\n\
    \      ws.send(JSON.stringify(a));\n\
    \    }, sync);\n\
    \    var syncKey = \"\";\n\
    \\n\
    \    ws.onopen = function(e) {\n\
    \\n\
    \        ws.onmessage = function(c) {\n\
    \            connId = c.data;\n\
    \            ws.onmessage = function(e) {\n\
    \                core.processReq(JSON.parse(e.data));\n\
    \            };\n\
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
