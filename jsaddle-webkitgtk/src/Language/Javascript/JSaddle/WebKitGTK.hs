{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.WebKitGTK
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | Run jsaddle apps in a GTK4 window hosting a WebKitGTK 6.0 WebView.
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

import Control.Monad (void)
import Control.Concurrent (forkIO, yield)

#ifndef mingw32_HOST_OS
import System.Posix.Signals (installHandler, keyboardSignal, Handler(Catch))
#endif
import System.Directory (getCurrentDirectory)

import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString.Lazy as LB (ByteString)
import Data.Aeson (encode, decode)
import qualified Data.Text as T (pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import GI.GLib
       (timeoutAdd, idleAdd, mainLoopNew, mainLoopRun, mainLoopQuit,
        pattern PRIORITY_HIGH, pattern PRIORITY_DEFAULT)
import qualified GI.Gtk as Gtk (init)
import GI.Gtk
       (Window, windowNew, windowSetDefaultSize, windowSetChild,
        windowPresent, windowDestroy, scrolledWindowNew,
        scrolledWindowSetChild, onWidgetDestroy)
import GI.Gio (Cancellable)
import GI.JavaScriptCore (valueToString)
import GI.WebKit
       (scriptDialogPromptSetText, scriptDialogPromptGetDefaultText,
        scriptDialogGetMessage, scriptDialogGetDialogType,
        onWebViewScriptDialog, WebView, webViewNew,
        setSettingsEnableWriteConsoleMessagesToStdout,
        setSettingsEnableJavascript,
        onUserContentManagerScriptMessageReceived,
        userContentManagerRegisterScriptMessageHandler,
        webViewGetUserContentManager,
        webViewEvaluateJavascript, LoadEvent(..), webViewLoadHtml,
        onWebViewLoadChanged, setSettingsEnableDeveloperExtras,
        webViewSetSettings, webViewGetSettings, ScriptDialogType(..))

import Language.Javascript.JSaddle (JSM, Results, Batch)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (initState, runBatch, ghcjsHelpers)

noCancellable :: Maybe Cancellable
noCancellable = Nothing

quitWindow :: Window -> IO ()
quitWindow window = postGUIAsync $ windowDestroy window

installQuitHandler :: Window -> IO ()
#ifdef mingw32_HOST_OS
installQuitHandler _ = return () -- TODO: Maybe figure something out here for Windows users.
#else
installQuitHandler window = void $ installHandler keyboardSignal (Catch (quitWindow window)) Nothing
#endif

postGUIAsync :: IO () -> IO ()
postGUIAsync action =
  void . idleAdd PRIORITY_DEFAULT $ action >> return False

run :: JSM () -> IO ()
run main = do
    Gtk.init
    loop <- mainLoopNew Nothing False
    _ <- timeoutAdd PRIORITY_HIGH 10 (yield >> return True)
    window <- windowNew
    windowSetDefaultSize window 900 600
    scrollWin <- scrolledWindowNew
    webView <- webViewNew
    settings <- webViewGetSettings webView
    setSettingsEnableDeveloperExtras settings True
    setSettingsEnableJavascript settings True
    setSettingsEnableWriteConsoleMessagesToStdout settings True
    webViewSetSettings webView settings
    scrolledWindowSetChild scrollWin (Just webView)
    windowSetChild window (Just scrollWin)
    _ <- onWidgetDestroy window $ mainLoopQuit loop
    windowPresent window
    pwd <- getCurrentDirectory
    let webViewLoadChangedCallback LoadEventFinished = runInWebView main webView
        webViewLoadChangedCallback _                 = return ()
    _ <- onWebViewLoadChanged webView webViewLoadChangedCallback
    webViewLoadHtml webView "" . Just $ "file://" <> T.pack pwd <> "/index.html"
    installQuitHandler window
    mainLoopRun loop

runInWebView :: JSM () -> WebView -> IO ()
runInWebView f webView = do
    (processResults, syncResults, start) <- runJavaScript (\batch -> postGUIAsync $
        webViewEvaluateJavascript webView
            (decodeUtf8 . toStrict $ "runJSaddleBatch(" <> encode batch <> ");")
            (-1) Nothing Nothing noCancellable Nothing)
        f

    addJSaddleHandler webView processResults syncResults
    webViewEvaluateJavascript webView (decodeUtf8 $ toStrict jsaddleJs)
        (-1) Nothing Nothing noCancellable . Just $
            \_obj _asyncResult ->
                void $ forkIO start

addJSaddleHandler :: WebView -> (Results -> IO ()) -> (Results -> IO Batch) -> IO ()
addJSaddleHandler webView processResult syncResults = do
    manager <- webViewGetUserContentManager webView
    _ <- onUserContentManagerScriptMessageReceived manager Nothing $ \value -> do
        bs <- encodeUtf8 <$> valueToString value
        mapM_ processResult (decode (fromStrict bs))
    _ <- onWebViewScriptDialog webView $ \dialog ->
        scriptDialogGetDialogType dialog >>= \case
            ScriptDialogTypePrompt ->
                scriptDialogGetMessage dialog >>= \case
                    "JSaddleSync" -> do
                        resultsText <- scriptDialogPromptGetDefaultText dialog
                        case decode (fromStrict $ encodeUtf8 resultsText) of
                            Just results -> do
                                batch <- syncResults results
                                scriptDialogPromptSetText dialog (decodeUtf8 . toStrict $ encode batch)
                                return True
                            Nothing -> return False
                    _ -> return False
            _ -> return False

    void $ userContentManagerRegisterScriptMessageHandler manager "jsaddle" Nothing

jsaddleJs :: LB.ByteString
jsaddleJs = ghcjsHelpers <> mconcat
    [ "runJSaddleBatch = (function() {\n"
    , initState
    , "\nreturn function(batch) {\n"
    , runBatch (\a -> "window.webkit.messageHandlers.jsaddle.postMessage(JSON.stringify(" <> a <> "));\n")
               (Just (\a -> "JSON.parse(window.prompt(\"JSaddleSync\", JSON.stringify(" <> a <> ")))"))
    , "};\n"
    , "})()"
    ]

#endif
