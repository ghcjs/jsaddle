{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Language.Javascript.JSaddle.WKWebView.Internal
    ( jsaddleMain
    , jsaddleMainHTMLWithBaseURL
    , jsaddleMainFile
    , WKWebView(..)
    , mainBundleResourcePath
    , registerJSaddleCallbacks
    ) where

import Control.Monad (void, join, unless)
import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.MVar
       (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar)

import System.IO.Unsafe (unsafePerformIO)

import Data.Monoid ((<>))
import Data.ByteString (useAsCString, packCString)
import qualified Data.ByteString as BS (ByteString)
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import Data.Aeson (encode, decode)
import qualified Data.Text as T (pack)
import Data.Text.Encoding (encodeUtf8)

import Foreign.C.String (CString)
import Foreign.C.Types (CBool(..), CInt(..))
import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr)

import Language.Javascript.JSaddle (Results, Batch, JSM)
import Language.Javascript.JSaddle.Run (runJavaScriptWithSerializer)
import Language.Javascript.JSaddle.Run.Files (initState, runBatch, ghcjsHelpers)

import System.Directory (getCurrentDirectory)

newtype WKWebView = WKWebView (Ptr WKWebView)
newtype JSaddleHandler = JSaddleHandler (Ptr JSaddleHandler)

-- | Serialises the jsaddle batch round-trip across every WKWebView in the
-- process.  All windows dispatch their JS onto the one Cocoa main queue, and a
-- synchronous @window.prompt@ round-trip blocks that queue; without this lock
-- two windows driving JS concurrently can wedge each other.  Shared (not
-- per-webview) precisely because the contended resource — the main queue — is
-- shared.  See 'runJavaScriptWithSerializer'.
{-# NOINLINE wkWebViewBatchLock #-}
wkWebViewBatchLock :: MVar ()
wkWebViewBatchLock = unsafePerformIO (newMVar ())

foreign export ccall jsaddleStart :: StablePtr (IO ()) -> IO ()
foreign export ccall jsaddleResult :: StablePtr (Results -> IO ()) -> CString -> IO ()
foreign export ccall jsaddleSyncResult :: StablePtr (Results -> IO Batch) -> JSaddleHandler -> CString -> IO ()
foreign export ccall callWithWebView :: WKWebView -> StablePtr (WKWebView -> IO ()) -> IO ()
foreign export ccall callWithCIntCString :: CInt -> CString -> StablePtr (CInt -> CString -> IO ()) -> IO ()
foreign export ccall callWithCString :: CString -> StablePtr (CString -> IO ()) -> IO ()
foreign export ccall callWithCStringReturningBool :: CString -> StablePtr (CString -> IO CBool) -> IO CBool
foreign export ccall callIO :: StablePtr (IO ()) -> IO ()

-- The Objective-C side calls back into Haskell through function pointers
-- registered with 'registerJSaddleCallbacks' rather than through the foreign
-- exports above (kept for compatibility): on toolchains where GHCi's RTS
-- linker loads this package's archive, the exports are not dyld-visible, so
-- ObjC compiled into a dylib (necessary there for its classes to be
-- registered with the ObjC runtime; see the objc-in-library cabal flag)
-- could not reference them.  See cbits/WKWebView-callbacks.h.
foreign import ccall "wrapper" mkStablePtrIOCb
  :: (StablePtr (IO ()) -> IO ())
  -> IO (FunPtr (StablePtr (IO ()) -> IO ()))
foreign import ccall "wrapper" mkResultCb
  :: (StablePtr (Results -> IO ()) -> CString -> IO ())
  -> IO (FunPtr (StablePtr (Results -> IO ()) -> CString -> IO ()))
foreign import ccall "wrapper" mkSyncResultCb
  :: (StablePtr (Results -> IO Batch) -> JSaddleHandler -> CString -> IO ())
  -> IO (FunPtr (StablePtr (Results -> IO Batch) -> JSaddleHandler -> CString -> IO ()))
foreign import ccall "wrapper" mkCStringCb
  :: (CString -> StablePtr (CString -> IO ()) -> IO ())
  -> IO (FunPtr (CString -> StablePtr (CString -> IO ()) -> IO ()))
foreign import ccall "wrapper" mkCStringBoolCb
  :: (CString -> StablePtr (CString -> IO CBool) -> IO CBool)
  -> IO (FunPtr (CString -> StablePtr (CString -> IO CBool) -> IO CBool))
foreign import ccall "wrapper" mkWebViewCb
  :: (WKWebView -> StablePtr (WKWebView -> IO ()) -> IO ())
  -> IO (FunPtr (WKWebView -> StablePtr (WKWebView -> IO ()) -> IO ()))
foreign import ccall "wrapper" mkCIntCStringCb
  :: (CInt -> CString -> StablePtr (CInt -> CString -> IO ()) -> IO ())
  -> IO (FunPtr (CInt -> CString -> StablePtr (CInt -> CString -> IO ()) -> IO ()))
foreign import ccall "jsaddle_wk_set_callbacks" c_jsaddleWkSetCallbacks
  :: FunPtr (StablePtr (IO ()) -> IO ())
  -> FunPtr (StablePtr (Results -> IO ()) -> CString -> IO ())
  -> FunPtr (StablePtr (Results -> IO Batch) -> JSaddleHandler -> CString -> IO ())
  -> FunPtr (StablePtr (IO ()) -> IO ())
  -> FunPtr (CString -> StablePtr (CString -> IO ()) -> IO ())
  -> FunPtr (CString -> StablePtr (CString -> IO CBool) -> IO CBool)
  -> FunPtr (WKWebView -> StablePtr (WKWebView -> IO ()) -> IO ())
  -> FunPtr (CInt -> CString -> StablePtr (CInt -> CString -> IO ()) -> IO ())
  -> IO ()

{-# NOINLINE callbacksRegistered #-}
callbacksRegistered :: MVar Bool
callbacksRegistered = unsafePerformIO (newMVar False)

-- | Register the Haskell callbacks with the Objective-C side.  Idempotent;
-- run before anything can trigger a callback (jsaddleMain' and run' call it).
-- The FunPtrs live for the life of the process.
registerJSaddleCallbacks :: IO ()
registerJSaddleCallbacks = modifyMVar_ callbacksRegistered $ \done -> do
    unless done $ do
        start  <- mkStablePtrIOCb jsaddleStart
        result <- mkResultCb jsaddleResult
        sync   <- mkSyncResultCb jsaddleSyncResult
        io     <- mkStablePtrIOCb callIO
        cstr   <- mkCStringCb callWithCString
        cstrb  <- mkCStringBoolCb callWithCStringReturningBool
        webv   <- mkWebViewCb callWithWebView
        cint   <- mkCIntCStringCb callWithCIntCString
        c_jsaddleWkSetCallbacks start result sync io cstr cstrb webv cint
    return True

foreign import ccall addJSaddleHandler :: WKWebView -> StablePtr (IO ()) -> StablePtr (Results -> IO ()) -> StablePtr (Results -> IO Batch) -> IO ()
foreign import ccall loadHTMLStringWithBaseURL :: WKWebView -> CString -> CString -> IO ()
foreign import ccall loadBundleFile :: WKWebView -> CString -> CString -> IO ()
foreign import ccall evaluateJavaScript :: WKWebView -> CString -> IO ()
foreign import ccall completeSync :: JSaddleHandler -> CString -> IO ()
foreign import ccall mainBundleResourcePathC :: IO CString

-- | Run JSaddle in WKWebView
jsaddleMain :: JSM () -> WKWebView -> IO ()
jsaddleMain f webView = do
    pwd <- getCurrentDirectory
    let baseURL = encodeUtf8 $ "file://" <> T.pack pwd <> "/index.html"
    jsaddleMainHTMLWithBaseURL (toStrict indexHtml) baseURL f webView

-- | Run JSaddle in WKWebView with initial html and base url
jsaddleMainHTMLWithBaseURL
  :: BS.ByteString -- ^ HTML
  -> BS.ByteString -- ^ Base URL
  -> JSM ()
  -> WKWebView
  -> IO ()
jsaddleMainHTMLWithBaseURL initialHTML baseURL f webView =
    jsaddleMain' f webView $
        useAsCString baseURL $ \url ->
            useAsCString initialHTML $ \html ->
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
    registerJSaddleCallbacks
    ready <- newEmptyMVar

    (processResult, syncResult, start) <- runJavaScriptWithSerializer (Just wkWebViewBatchLock) (\batch ->
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

callWithCIntCString :: CInt -> CString -> StablePtr (CInt -> CString -> IO ()) -> IO ()
callWithCIntCString n s fptr = do
    f <- deRefStablePtr fptr
    f n s

callWithCStringReturningBool :: CString -> StablePtr (CString -> IO CBool) -> IO CBool
callWithCStringReturningBool c fptr = do
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
