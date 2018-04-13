{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Language.Javascript.JSaddle.WKWebView.Internal
    ( jsaddleMain
    , jsaddleMainFile
    , WKWebView(..)
    , mainBundleResourcePath
    ) where

import Control.Monad (void, join)
import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import Data.Monoid ((<>))
import Data.ByteString (useAsCString, packCString)
import qualified Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import Data.Aeson (encode, decode)

import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr)

import Language.Javascript.JSaddle (Results, Batch, JSM)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (initState, runBatch, ghcjsHelpers)

newtype WKWebView = WKWebView (Ptr WKWebView)
newtype JSaddleHandler = JSaddleHandler (Ptr JSaddleHandler)

foreign export ccall jsaddleStart :: StablePtr (IO ()) -> IO ()
foreign export ccall jsaddleResult :: StablePtr (Results -> IO ()) -> CString -> IO ()
foreign export ccall jsaddleSyncResult :: StablePtr (Results -> IO Batch) -> JSaddleHandler -> CString -> IO ()
foreign export ccall callWithWebView :: WKWebView -> StablePtr (WKWebView -> IO ()) -> IO ()
foreign export ccall callWithCString :: CString -> StablePtr (CString -> IO ()) -> IO ()
foreign export ccall callIO :: StablePtr (IO ()) -> IO ()
foreign import ccall addJSaddleHandler :: WKWebView -> StablePtr (IO ()) -> StablePtr (Results -> IO ()) -> StablePtr (Results -> IO Batch) -> IO ()
foreign import ccall loadHTMLStringWithBaseURL :: WKWebView -> CString -> CString -> IO ()
foreign import ccall loadBundleFile :: WKWebView -> CString -> CString -> IO ()
foreign import ccall evaluateJavaScript :: WKWebView -> CString -> IO ()
foreign import ccall completeSync :: JSaddleHandler -> CString -> IO ()
foreign import ccall mainBundleResourcePathC :: IO CString

-- | Run JSaddle in WKWebView
jsaddleMain :: JSM () -> WKWebView -> IO ()
jsaddleMain f webView =
    jsaddleMain' f webView $
        useAsCString (toStrict indexHtmlBaseURL) $ \url ->
            useAsCString (toStrict indexHtml) $ \html ->
                loadHTMLStringWithBaseURL webView html url

-- | Run JSaddle in a WKWebView first loading the specified file
--   from the mainBundle (relative to the resourcePath).
jsaddleMainFile :: BS.ByteString -- ^ The file to navigate to.
                -> BS.ByteString -- ^ The path to allow read access to.
                -> JSM () -> WKWebView -> IO ()
jsaddleMainFile url allowing f webView =
    jsaddleMain' f webView $
        useAsCString url $ \u ->
            useAsCString allowing $ \a ->
                loadBundleFile webView u a

jsaddleMain' :: JSM () -> WKWebView -> IO () -> IO ()
jsaddleMain' f webView loadHtml = do
    ready <- newEmptyMVar

    (processResult, syncResult, start) <- runJavaScript (\batch ->
        useAsCString (toStrict $ "runJSaddleBatch(" <> encode batch <> ");") $
            evaluateJavaScript webView)
        f

    startHandler <- newStablePtr (putMVar ready ())
    resultHandler <- newStablePtr processResult
    syncResultHandler <- newStablePtr syncResult
    addJSaddleHandler webView startHandler resultHandler syncResultHandler
    loadHtml
    void . forkOS $ do
        takeMVar ready
        useAsCString (toStrict jsaddleJs) (evaluateJavaScript webView) >> void (forkIO start)

jsaddleStart :: StablePtr (IO ()) -> IO ()
jsaddleStart ptrHandler = join (deRefStablePtr ptrHandler)

jsaddleResult :: StablePtr (Results -> IO ()) -> CString -> IO ()
jsaddleResult ptrHandler s = do
    processResult <- deRefStablePtr ptrHandler
    result <- packCString s
    case decode (fromStrict result) of
        Nothing -> error $ "jsaddle Results decode failed : " <> show result
        Just r  -> processResult r

jsaddleSyncResult :: StablePtr (Results -> IO Batch) -> JSaddleHandler -> CString -> IO ()
jsaddleSyncResult ptrHandler jsaddleHandler s = do
    syncProcessResult <- deRefStablePtr ptrHandler
    result <- packCString s
    case decode (fromStrict result) of
        Nothing -> error $ "jsaddle Results decode failed : " <> show result
        Just r  -> do -- void . forkIO $ do
            batch <- syncProcessResult r
            useAsCString (toStrict $ encode batch) $
                completeSync jsaddleHandler

callWithWebView :: WKWebView -> StablePtr (WKWebView -> IO ()) -> IO ()
callWithWebView webView ptrF = do
    f <- deRefStablePtr ptrF
    f webView

callWithCString :: CString -> StablePtr (CString -> IO ()) -> IO ()
callWithCString c fptr = do
    f <- deRefStablePtr fptr
    f c

callIO :: StablePtr (IO ()) -> IO ()
callIO ptr = do
    f <- deRefStablePtr ptr
    f

jsaddleJs :: ByteString
jsaddleJs = ghcjsHelpers <> "\
    \runJSaddleBatch = (function() {\n\
    \ " <> initState <> "\n\
    \ return function(batch) {\n\
    \ " <> runBatch (\a -> "window.webkit.messageHandlers.jsaddle.postMessage(JSON.stringify(" <> a <> "));\n")
                    (Just (\a -> "JSON.parse(window.prompt(\"JSaddleSync\", JSON.stringify(" <> a <> ")))")) <> "\
    \ };\n\
    \})();\n\
    \"


indexHtmlBaseURL :: ByteString
indexHtmlBaseURL = "file:///tmp/index.html"

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

mainBundleResourcePath :: IO (Maybe BS.ByteString)
mainBundleResourcePath = do
    bs <- mainBundleResourcePathC
    if bs == nullPtr
        then return Nothing
        else Just <$> packCString bs
