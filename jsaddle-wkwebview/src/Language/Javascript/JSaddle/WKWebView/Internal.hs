{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Language.Javascript.JSaddle.WKWebView.Internal
    ( jsaddleMain
    , WKWebView(..)
    ) where

import Control.Monad (void, join)
import Control.Concurrent (forkIO)

import Data.Monoid ((<>))
import Data.ByteString (useAsCString, packCString)
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import Data.Aeson (encode, decode)

import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr)

import Language.Javascript.JSaddle (Results, JSM)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (initState, runBatch, ghcjsHelpers)

newtype WKWebView = WKWebView (Ptr WKWebView)

foreign export ccall jsaddleStart :: StablePtr (IO ()) -> IO ()
foreign export ccall jsaddleResult :: StablePtr (Results -> IO ()) -> CString -> IO ()
foreign export ccall withWebView :: WKWebView -> StablePtr (WKWebView -> IO ()) -> IO ()
foreign import ccall addJSaddleHandler :: WKWebView -> StablePtr (IO ()) -> StablePtr (Results -> IO ()) -> IO ()
foreign import ccall loadHTMLString :: WKWebView -> CString -> IO ()
foreign import ccall evaluateJavaScript :: WKWebView -> CString -> IO ()

jsaddleMain :: JSM () -> WKWebView -> IO ()
jsaddleMain f webView = do
    (processResult, start) <- runJavaScript (\batch ->
        useAsCString (toStrict $ "runJSaddleBatch(" <> encode batch <> ");") $
            evaluateJavaScript webView)
        f

    startHandler <- newStablePtr (void $ forkIO start)
    resultHandler <- newStablePtr processResult
    addJSaddleHandler webView startHandler resultHandler

    useAsCString (toStrict indexHtml) $ loadHTMLString webView
    useAsCString (toStrict jsaddleJs) $ evaluateJavaScript webView

jsaddleStart :: StablePtr (IO ()) -> IO ()
jsaddleStart ptrHandler = join (deRefStablePtr ptrHandler)

jsaddleResult :: StablePtr (Results -> IO ()) -> CString -> IO ()
jsaddleResult ptrHandler s = do
    processResult <- deRefStablePtr ptrHandler
    result <- packCString s
    case decode (fromStrict result) of
        Nothing -> error $ "jsaddle WebSocket decode failed : " <> show result
        Just r  -> processResult r

withWebView :: WKWebView -> StablePtr (WKWebView -> IO ()) -> IO ()
withWebView webView ptrF = do
    f <- deRefStablePtr ptrF
    f webView

jsaddleJs :: ByteString
jsaddleJs = ghcjsHelpers <> "\
    \runJSaddleBatch = (function() {\n\
    \ " <> initState <> "\n\
    \ return function(batch) {\n\
    \ " <> runBatch (\a -> "window.webkit.messageHandlers.jsaddle.postMessage(JSON.stringify(" <> a <> "));") <> "\
    \ };\n\
    \})();\n\
    \"

indexHtml :: ByteString
indexHtml =
    "<!DOCTYPE html>\n\
    \<html>\n\
    \<head>\n\
    \<title>JSaddle</title>\n\
    \</head>\n\
    \<body>\n\
    \</body>\n\
    \</html>"
