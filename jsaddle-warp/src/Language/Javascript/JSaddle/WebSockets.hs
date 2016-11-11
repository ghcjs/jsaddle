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
) where

import Control.Monad (forever)
import Control.Concurrent (forkIO)

import Data.Monoid ((<>))
import Data.Aeson (encode, decode)

import Network.Wai (Application)
import Network.WebSockets
       (ConnectionOptions(..), sendTextData, receiveDataMessage,
        acceptRequest, ServerApp)
import qualified Network.WebSockets as WS (DataMessage(..))
import Network.Wai.Handler.WebSockets (websocketsOr)

import Language.Javascript.JSaddle.Types (JSM(..))
import qualified Network.Wai as W
       (responseLBS, requestMethod, Application, pathInfo)
import Data.Text (Text)
import qualified Network.HTTP.Types as H
       (status403, status200, status405)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (indexHtml, jsaddleJs)

jsaddleOr :: ConnectionOptions -> JSM () -> Application -> Application
jsaddleOr opts entryPoint = websocketsOr opts wsApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
        conn <- acceptRequest pending_conn
        (processResult, start) <- runJavaScript (sendTextData conn . encode) entryPoint
        _ <- forkIO . forever $
            receiveDataMessage conn >>= \case
                (WS.Text t) ->
                    case decode t of
                        Nothing -> error $ "jsaddle WebSocket decode failed : " <> show t
                        Just r  -> processResult r
                _ -> error "jsaddle WebSocket unexpected binary data"
        start

jsaddleApp :: Application
jsaddleApp req = jsaddleAppPieces (W.pathInfo req) req

jsaddleAppPieces :: [Text] -> W.Application
jsaddleAppPieces _ req sendResponse
    | W.requestMethod req `notElem` ["GET", "HEAD"] = sendResponse $ W.responseLBS
        H.status405
        [("Content-Type", "text/plain")]
        "Only GET or HEAD is supported"
jsaddleAppPieces [] _req sendResponse = sendResponse $ W.responseLBS H.status200 [("Content-Type", "text/html")] indexHtml
jsaddleAppPieces ["jsaddle.js"] _req sendResponse = sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/javascript")] jsaddleJs
jsaddleAppPieces _rawPieces _req sendResponse = sendResponse $ W.responseLBS H.status403
            [ ("Content-Type", "text/plain")
            ] "Forbidden"
