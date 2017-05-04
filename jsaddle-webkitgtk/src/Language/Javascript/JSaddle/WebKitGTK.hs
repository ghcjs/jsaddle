{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
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
module Language.Javascript.JSaddle.WebKitGTK (
  -- * Running JSM in a WebView
    run
#ifndef ghcjs_HOST_OS
  , runInWebView
#endif
  ) where

#ifdef ghcjs_HOST_OS

run :: IO () -> IO ()
run = id
{-# INLINE run #-}

#else

import Control.Monad (when, void)
import qualified Control.Exception as E (catch)
import Control.Exception (SomeException(..))
import Control.Concurrent (forkIO, yield)

#ifndef mingw32_HOST_OS
import System.Posix.Signals (installHandler, keyboardSignal, Handler(Catch))
#endif
import System.Directory (getCurrentDirectory)

import Data.Monoid ((<>))
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString.Lazy as LB (ByteString)
import Data.Aeson (encode, decode)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import Data.Text.Foreign (useAsPtr, fromPtr)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Marshal.Array (withArray, peekArray)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)

import Data.GI.Base.ManagedPtr (withManagedPtr)
import GI.GLib
       (timeoutAdd, idleAdd, pattern PRIORITY_HIGH, pattern PRIORITY_DEFAULT)
import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk
       (windowSetPosition, windowSetDefaultSize, windowNew, scrolledWindowNew,
        noAdjustment, containerAdd, WindowType(..), WindowPosition(..),
        widgetDestroy, widgetGetToplevel, widgetShowAll, onWidgetDestroy,
        mainQuit)
import GI.JavaScriptCore.Structs.GlobalContext (GlobalContext(..))
import GI.WebKit
       (WebView, webViewGetMainFrame, webFrameGetGlobalContext,
        onWebInspectorInspectWebView, webViewGetInspector, webViewSetSettings,
        setWebSettingsEnableDeveloperExtras,
        setWebSettingsEnableUniversalAccessFromFileUris,
        webViewGetSettings, webViewNew, onWebViewLoadFinished,
        webViewLoadString)

import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSValueRef, JSValueRefRef, JSObjectRef, JSContextRef,
        jsevaluatescript, JSStringRef)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
       (jsstringretain, jsstringrelease, jsstringgetcharactersptr,
        jsstringgetlength, jsstringcreatewithcharacters)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
       (jsobjectcallasfunction, JSCSize, jsobjectsetproperty,
        jsobjectmakefunctionwithcallback, mkJSObjectCallAsFunctionCallback)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSContextRef
       (jscontextgetglobalobject)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
       (jsvalueprotect, jsvalueunprotect,
        jsvaluetostringcopy, jsvaluemakestring, jsvaluemakeundefined)

import Language.Javascript.JSaddle (JSM, Results)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (initState, runBatch, ghcjsHelpers)

quitWebView :: WebView -> IO ()
quitWebView wv = postGUIAsync $ do w <- widgetGetToplevel wv --TODO: Shouldn't this be postGUISync?
                                   widgetDestroy w

installQuitHandler :: WebView -> IO ()
#ifdef mingw32_HOST_OS
installQuitHandler wv = return () -- TODO: Maybe figure something out here for Windows users.
#else
installQuitHandler wv = void $ installHandler keyboardSignal (Catch (quitWebView wv)) Nothing
#endif

postGUIAsync :: IO () -> IO ()
postGUIAsync action =
  void . idleAdd PRIORITY_DEFAULT $ action >> return False

withJSString :: Text -> (JSStringRef -> IO a) -> IO a
withJSString s f = do
    jsstring <- useAsPtr s $ \p l -> jsstringcreatewithcharacters (castPtr p) (fromIntegral l)
    _ <- jsstringretain jsstring
    result <- f jsstring
    jsstringrelease jsstring
    return result

withJSContextRef :: GlobalContext -> (JSContextRef -> IO a) -> IO a
withJSContextRef (GlobalContext ctx) f = withManagedPtr ctx (f . castPtr)

valueToText :: JSContextRef -> JSValueRef -> IO Text
valueToText ctxRef s = do
    jsstring <- jsvaluetostringcopy ctxRef s nullPtr
    l <- jsstringgetlength jsstring
    p <- jsstringgetcharactersptr jsstring
    result <- fromPtr (castPtr p) (fromIntegral l)
    jsstringrelease jsstring
    return result

check :: JSContextRef -> (JSValueRefRef -> IO a) -> IO a
check ctxRef f =
    alloca $ \exceptionPtr -> do
        poke exceptionPtr nullPtr
        result <- f exceptionPtr
        exception <- peek exceptionPtr
        when (exception /= nullPtr) $ do
            errorText <- valueToText ctxRef exception
            error $ T.unpack errorText
        return result

eval :: JSContextRef -> Text -> IO JSValueRef
eval ctxRef script =
    withJSString script $ \script' ->
        check ctxRef $ jsevaluatescript ctxRef script' nullPtr nullPtr 0

run :: JSM () -> IO ()
run main = do
    _ <- Gtk.init Nothing
    window <- windowNew WindowTypeToplevel
    _ <- timeoutAdd PRIORITY_HIGH 10 (yield >> return True)
    windowSetDefaultSize window 900 600
    windowSetPosition window WindowPositionCenter
    scrollWin <- scrolledWindowNew noAdjustment noAdjustment
    webView <- webViewNew
    settings <- webViewGetSettings webView
    setWebSettingsEnableUniversalAccessFromFileUris settings True
    setWebSettingsEnableDeveloperExtras settings True
    webViewSetSettings webView settings
    window `containerAdd` scrollWin
    scrollWin `containerAdd` webView
    _ <- onWidgetDestroy window mainQuit
    widgetShowAll window
    inspector <- webViewGetInspector webView
    _ <- onWebInspectorInspectWebView inspector $ \_ -> do
        inspectorWindow <- windowNew WindowTypeToplevel
        windowSetDefaultSize inspectorWindow 900 600
        inspectorScrollWin <- scrolledWindowNew noAdjustment noAdjustment
        inspectorWebView <- webViewNew
        inspectorWindow `containerAdd` inspectorScrollWin
        inspectorScrollWin `containerAdd` inspectorWebView
        widgetShowAll inspectorWindow
        return inspectorWebView
    pwd <- getCurrentDirectory
    void . onWebViewLoadFinished webView $ \ _ ->
        runInWebView main webView
    webViewLoadString webView "" "text/html" "UTF-8" $ "file://" <> T.pack pwd <> "/"
    installQuitHandler webView
    Gtk.main

runInWebView :: JSM () -> WebView -> IO ()
runInWebView f webView = do
    ctx <- webViewGetMainFrame webView >>= webFrameGetGlobalContext
    runJSaddleBatch <-
        withJSContextRef ctx $ \ctxRef -> do
            result <- eval ctxRef (decodeUtf8 $ toStrict jsaddleJs)
            jsvalueprotect ctxRef result
            return result

    (processResults, _, start) <- runJavaScript (\batch -> postGUIAsync $
        withJSContextRef ctx $ \ctxRef ->
            withJSString (decodeUtf8 . toStrict $ encode batch) $ \script -> do
                val <- jsvaluemakestring ctxRef script
                withArray [val] $ \args ->
                    void . check ctxRef $ jsobjectcallasfunction ctxRef runJSaddleBatch nullPtr 1 args
                jsvalueunprotect ctxRef val)
        f

    withJSContextRef ctx $ \ctxRef -> do
        addJSaddleHandler ctxRef processResults
        void $ forkIO start

addJSaddleHandler :: JSContextRef -> (Results -> IO ()) -> IO ()
addJSaddleHandler ctxRef processResult = do
    callback <- mkJSObjectCallAsFunctionCallback wrap
    function <- jsobjectmakefunctionwithcallback ctxRef nullPtr callback
    global <- jscontextgetglobalobject ctxRef
    withJSString "jsaddleCallback" $ \propName ->
        jsobjectsetproperty ctxRef global propName function 0 nullPtr
  where
    wrap :: JSContextRef -> JSObjectRef -> JSObjectRef -> JSCSize -> JSValueRefRef -> JSValueRefRef -> IO JSValueRef
    wrap _ctx _fobj _this argc argv exception = do
            [arg] <- peekArray (fromIntegral argc) argv
            bs <- encodeUtf8 <$> valueToText ctxRef arg
            mapM_ processResult (decode (fromStrict bs))
            jsvaluemakeundefined ctxRef
      `E.catch` \(e :: SomeException) -> do
            withJSString (T.pack (show e)) (jsvaluemakestring ctxRef) >>= poke exception
            jsvaluemakeundefined ctxRef

jsaddleJs :: LB.ByteString
jsaddleJs = ghcjsHelpers <> mconcat
    [ "(function() {\n"
    , initState
    , "\nreturn function(batchJSON) {\n"
    , "  var batch = JSON.parse(batchJSON);\n"
    , runBatch (\a -> "window.jsaddleCallback(JSON.stringify(" <> a <> "));\n") Nothing
    , "};\n"
    , "})()\n"
    ]

#endif
