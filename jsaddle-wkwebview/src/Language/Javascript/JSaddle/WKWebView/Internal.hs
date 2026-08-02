{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, ScopedTypeVariables #-}
module Language.Javascript.JSaddle.WKWebView.Internal
    ( jsaddleMain
    , jsaddleMainHTMLWithBaseURL
    , jsaddleMainFile
    , WKWebView(..)
    , mainBundleResourcePath
    , registerJSaddleCallbacks
    , jsaddleWebViewInvalidate
    ) where

import Control.Monad (void, join, unless)
import Control.Concurrent (forkIO, forkOS)
import Control.Exception (SomeException, handle)
import Control.Concurrent.MVar
       (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar,
        withMVar)

import Data.IORef (newIORef, readIORef, writeIORef, atomicModifyIORef')

import System.IO (hPutStrLn, stderr)
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
import Foreign.StablePtr
       (StablePtr, newStablePtr, deRefStablePtr, freeStablePtr)

import Language.Javascript.JSaddle (Results, JSM)
import Language.Javascript.JSaddle.Types (Batch(..))
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

-- | Per-context invalidators, keyed by the raw webview pointer each context
-- sends its batches to.  'jsaddleWebViewInvalidate' runs (and drops) a
-- webview's entries so the host application can safely release the webview:
-- until then jsaddle's threads keep calling 'evaluateJavaScript' with the
-- stored pointer, and releasing the webview under them is a use-after-free.
{-# NOINLINE contextInvalidators #-}
contextInvalidators :: MVar [(Ptr WKWebView, IO ())]
contextInvalidators = unsafePerformIO (newMVar [])

-- | Invalidate every jsaddle context attached to @webView@: no jsaddle thread
-- will touch the webview pointer after this returns, so the host may release
-- the webview (and let its WebContent renderer process exit).  Blocks until
-- any in-flight batch send has completed.  The contexts' Haskell threads are
-- not stopped — their subsequent sends throw instead (a dropped send would
-- leave jsaddle's batch thread waiting for a reply forever while holding
-- 'wkWebViewBatchLock', deadlocking every other window; see 'jsaddleMain'').
--
-- Also detaches each context's ObjC handler and frees the stable pointers it
-- holds, which is what makes an invalidated context COLLECTABLE — see
-- @removeJSaddleHandler@ in cbits/WKWebView-cbits.m.  Without it the callback
-- closures, and through them the context's entire object graph, are pinned for
-- the life of the process.
jsaddleWebViewInvalidate :: WKWebView -> IO ()
jsaddleWebViewInvalidate (WKWebView ptr) =
    modifyMVar_ contextInvalidators $ \invs -> do
        sequence_ [ inv | (p, inv) <- invs, p == ptr ]
        return (filter ((/= ptr) . fst) invs)

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

foreign import ccall addJSaddleHandler :: WKWebView -> StablePtr (IO ()) -> StablePtr (Results -> IO ()) -> StablePtr (Results -> IO Batch) -> IO JSaddleHandler
foreign import ccall removeJSaddleHandler :: WKWebView -> JSaddleHandler -> StablePtr (IO ()) -> IO ()
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
jsaddleMain' f webView@(WKWebView ptr) loadHtml = do
    registerJSaddleCallbacks
    ready <- newEmptyMVar

    -- Every use of the raw webview pointer goes through this gate.  An MVar
    -- (not an IORef) so 'jsaddleWebViewInvalidate' SYNCHRONIZES with an
    -- in-flight send: invalidation blocks until the send's evaluateJavaScript
    -- call — which copies the pointer into a main-queue block, retaining it —
    -- has returned, and no later send can start.  After that the host can
    -- release the webview with no thread left to dereference it.
    aliveVar <- newMVar True
    -- This context's pending detach (see @removeJSaddleHandler@), from the
    -- moment 'addJSaddleHandler' has installed something to detach until
    -- invalidation runs it.  Taken and written under @aliveVar@ — by the
    -- invalidator below and by the install site — so the detach happens exactly
    -- once even if the context is invalidated while it is still being set up.
    detachRef <- newIORef Nothing
    modifyMVar_ contextInvalidators $ return .
        ((ptr, modifyMVar_ aliveVar $ \_ -> do
                   mbDetach <- atomicModifyIORef' detachRef (\d -> (Nothing, d))
                   sequence_ mbDetach
                   return False) :)
    -- An invalidated context must FAIL its send, not silently drop it.
    -- 'runJavaScriptWithSerializer' holds the shared 'wkWebViewBatchLock'
    -- across @sendBatch@ AND the @takeResult@ that waits for that batch's
    -- reply: a dropped batch leaves its batch thread waiting for a reply that
    -- can never arrive, holding the lock every OTHER window needs — so one
    -- invalidated context deadlocks the whole process (in ghci mode: every
    -- later @:main@ hangs on its first round trip, CPU idle).  Throwing
    -- unwinds out of 'withMVar', releasing both MVars, and kills only the dead
    -- context's own batch pump.
    let withWebView act = withMVar aliveVar $ \alive ->
            if alive then act
            else ioError (userError "jsaddle-wkwebview: context invalidated \
                                    \(webview released) — batch dropped")

    (processResult, syncResult, start) <- runJavaScriptWithSerializer (Just wkWebViewBatchLock) (\batch ->
        withWebView $
            useAsCString (toStrict $ "runJSaddleBatch(" <> encode batch <> ");") $
                evaluateJavaScript webView)
        f

    -- INBOUND must be gated too, or invalidating a context leaks threads without
    -- bound.  The webview keeps its script-message handler until the host
    -- releases it, and a page whose JS is still running keeps posting results;
    -- 'runJavaScriptWithSerializer' forks a @processResults@ thread per delivery,
    -- and with this context's batch pump gone (killed by the send gate above)
    -- every one of them blocks forever on the pump's MVar.  Measured before this
    -- gate: ~5000 blocked threads PER SECOND, 500k threads and 12GB RSS in one
    -- session (and 72GB in a longer one).  A dead context simply drops what
    -- arrives: nothing is listening for it anyway.
    --
    -- The flag is only READ here (not held across the handler): holding it would
    -- deadlock the sync path, whose 'readMVar lastAsyncBatch' can wait on the
    -- batch thread — which needs this same MVar to send.
    let ifAlive :: a -> IO a -> IO a
        ifAlive dflt act = readMVar aliveVar >>= \alive ->
            if alive then act else return dflt
    startHandler <- newStablePtr (putMVar ready ())
    resultHandler <- newStablePtr (\results -> ifAlive () (processResult results))
    -- An empty batch is what a context with nothing to say returns.
    syncResultHandler <- newStablePtr
        (\results -> ifAlive (Batch [] False 0) (syncResult results))
    handler <- addJSaddleHandler webView startHandler resultHandler syncResultHandler
    -- Freeing the three stable pointers above is what lets an invalidated
    -- context be collected: each pins its closure, and those closures capture
    -- the whole 'JSContextRef' — for a reflex application, the entire live
    -- network.  Held for the life of the process (as they were), every instance
    -- a ghci session ever ran stays reachable: ~30MB of live heap per reload.
    -- Only safe once the ObjC handler can no longer dereference them, so it is
    -- @removeJSaddleHandler@ that runs this, after detaching (asynchronously, on
    -- the main queue).  @selfRef@ lets the callback free its own stable pointer
    -- too; it is written before any detach can be requested below.
    selfRef <- newIORef Nothing
    onDetached <- newStablePtr $ do
        freeStablePtr startHandler
        freeStablePtr resultHandler
        freeStablePtr syncResultHandler
        readIORef selfRef >>= mapM_ freeStablePtr
        -- Traced because the detach is asynchronous: this line is the only
        -- evidence of WHEN (or whether) a context actually became collectable.
        hPutStrLn stderr $ "jsaddle-wkwebview: context detached from webview "
                           <> show ptr <> ", stable pointers freed"
    writeIORef selfRef (Just onDetached)
    -- Arm the detach for the invalidator — or, if invalidation already happened
    -- (it found nothing to detach), detach right here.
    let detach = removeJSaddleHandler webView handler onDetached
    modifyMVar_ aliveVar $ \alive -> do
        if alive then writeIORef detachRef (Just detach) else detach
        return alive
    loadHtml
    -- If the context is invalidated before the page signals ready, the gate
    -- throws here; that just means this window died during startup, so swallow
    -- it rather than letting forkOS report an unhandled exception.
    void . forkOS . handle (\(_ :: SomeException) -> return ()) $ do
        takeMVar ready
        withWebView $ useAsCString (toStrict jsaddleJs) (evaluateJavaScript webView)
        void (forkIO start)

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
