{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
-- | Wiring between jsaddle's 'runJavaScript' transport and the WebView2
--   C shim (@cbits/WebView2Shim.c@).  Mirrors
--   "Language.Javascript.JSaddle.WKWebView.Internal".
module Language.Javascript.JSaddle.WebView2.Internal
    ( jsaddleMain
    , jsaddleMainHTML
    , jsaddleMainURL
    , WebView2(..)
    ) where

import Control.Monad (void, join)
import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import Data.ByteString (useAsCString)
import qualified Data.ByteString as BS (ByteString, packCString)
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import Data.Aeson (encode, decode)

import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr)

import Language.Javascript.JSaddle (Results, Batch, JSM)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (initState, runBatch, ghcjsHelpers)

newtype WebView2 = WebView2 (Ptr WebView2)

foreign export ccall jsaddleStart :: StablePtr (IO ()) -> IO ()
foreign export ccall jsaddleResult :: StablePtr (Results -> IO ()) -> CString -> IO ()
foreign export ccall jsaddleSyncResult :: StablePtr (Results -> IO Batch) -> WebView2 -> CString -> IO ()
foreign export ccall callWithWebView2 :: WebView2 -> StablePtr (WebView2 -> IO ()) -> IO ()

foreign import ccall wv2SetHandlers
    :: WebView2 -> StablePtr (IO ()) -> StablePtr (Results -> IO ()) -> StablePtr (Results -> IO Batch) -> IO ()
foreign import ccall wv2Eval :: WebView2 -> CString -> IO ()
foreign import ccall wv2Navigate :: WebView2 -> CString -> IO ()
foreign import ccall wv2NavigateToString :: WebView2 -> CString -> IO ()
foreign import ccall completeSyncWV2 :: WebView2 -> CString -> IO ()

-- | Run a jsaddle app in the WebView2, starting from a minimal blank page.
jsaddleMain :: JSM () -> WebView2 -> IO ()
jsaddleMain = jsaddleMainHTML (toStrict indexHtml)

-- | Run a jsaddle app in the WebView2, starting from the given HTML
--   (loaded with @NavigateToString@, so the page origin is @about:blank@
--   and the content is limited to 2MB).
jsaddleMainHTML :: BS.ByteString -> JSM () -> WebView2 -> IO ()
jsaddleMainHTML html f webView =
    jsaddleMain' f webView $
        useAsCString html $ wv2NavigateToString webView

-- | Run a jsaddle app in the WebView2, first navigating to the given URL
--   (@file://@, @https://@, ...).
jsaddleMainURL :: BS.ByteString -> JSM () -> WebView2 -> IO ()
jsaddleMainURL url f webView =
    jsaddleMain' f webView $
        useAsCString url $ wv2Navigate webView

jsaddleMain' :: JSM () -> WebView2 -> IO () -> IO ()
jsaddleMain' f webView navigate = do
    ready <- newEmptyMVar

    (processResult, syncResult, start) <- runJavaScript (\batch ->
        useAsCString (toStrict $ "runJSaddleBatch(" <> encode batch <> ");") $
            wv2Eval webView)
        f

    startHandler <- newStablePtr (putMVar ready ())
    resultHandler <- newStablePtr processResult
    syncResultHandler <- newStablePtr syncResult
    wv2SetHandlers webView startHandler resultHandler syncResultHandler
    navigate
    void . forkOS $ do
        takeMVar ready
        useAsCString (toStrict jsaddleJs) (wv2Eval webView) >> void (forkIO start)

jsaddleStart :: StablePtr (IO ()) -> IO ()
jsaddleStart ptrHandler = join (deRefStablePtr ptrHandler)

jsaddleResult :: StablePtr (Results -> IO ()) -> CString -> IO ()
jsaddleResult ptrHandler s = do
    processResult <- deRefStablePtr ptrHandler
    result <- BS.packCString s
    case decode (fromStrict result) of
        Nothing -> error $ "jsaddle Results decode failed : " <> show result
        Just r  -> processResult r

-- | Called from the ScriptDialogOpening handler while the page's JS is
--   blocked in @window.prompt("JSaddleSync", ...)@.  The reply Batch must be
--   handed back with 'completeSyncWV2' before this returns (the C side reads
--   it right after).
jsaddleSyncResult :: StablePtr (Results -> IO Batch) -> WebView2 -> CString -> IO ()
jsaddleSyncResult ptrHandler webView s = do
    syncProcessResult <- deRefStablePtr ptrHandler
    result <- BS.packCString s
    case decode (fromStrict result) of
        Nothing -> error $ "jsaddle Results decode failed : " <> show result
        Just r  -> do
            batch <- syncProcessResult r
            useAsCString (toStrict $ encode batch) $
                completeSyncWV2 webView

callWithWebView2 :: WebView2 -> StablePtr (WebView2 -> IO ()) -> IO ()
callWithWebView2 webView ptrF = do
    f <- deRefStablePtr ptrF
    f webView

jsaddleJs :: ByteString
jsaddleJs = ghcjsHelpers <> "\
    \runJSaddleBatch = (function() {\n\
    \ " <> initState <> "\n\
    \ return function(batch) {\n\
    \ " <> runBatch (\a -> "window.chrome.webview.postMessage(JSON.stringify(" <> a <> "));\n")
                    (Just (\a -> "JSON.parse(window.prompt(\"JSaddleSync\", JSON.stringify(" <> a <> ")))")) <> "\
    \ };\n\
    \})();\n\
    \"

indexHtml :: ByteString
indexHtml =
    "<!DOCTYPE html>\n\
    \<html>\n\
    \<head>\n\
    \<meta charset=\"utf-8\">\n\
    \<title>JSaddle</title>\n\
    \</head>\n\
    \<body>\n\
    \</body>\n\
    \</html>"
