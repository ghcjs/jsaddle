{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
--
-- Module      :  TestJSC
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Prelude hiding((!!))
import Graphics.UI.Gtk
       (Window, widgetDestroy, postGUIAsync, postGUISync, widgetShowAll,
        mainGUI, mainQuit, onDestroy, containerAdd, scrolledWindowNew,
        windowSetPosition, windowSetDefaultSize, timeoutAddFull, windowNew,
        initGUI)
import Control.Concurrent
       (tryTakeMVar, forkIO, newMVar, putMVar, takeMVar, newEmptyMVar,
        yield)
import System.Glib.MainLoop (priorityHigh)
import Graphics.UI.Gtk.General.Enums (WindowPosition(..))
import Graphics.UI.Gtk.WebKit.WebView
       (webViewGetMainFrame, webViewNew)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans.Reader (runReaderT)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase (JSContextRef)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.WebFrame
       (webFrameGetGlobalContext)
import Language.Javascript.JSC
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative
import Control.Monad.IO.Class (MonadIO(..))

data TestState = TestState { jsContext :: JSContextRef, window :: Window }

state = unsafePerformIO $ newMVar Nothing
done = unsafePerformIO $ newEmptyMVar

runjs f = do
    tryTakeMVar done
    mbState <- takeMVar state
    TestState {..} <- case mbState of
        Nothing -> do
            newState <- newEmptyMVar
            forkIO $ do
                initGUI
                window <- windowNew
                timeoutAddFull (yield >> return True) priorityHigh 10
--                windowSetDefaultSize window 900 600
--                windowSetPosition window WinPosCenter
                scrollWin <- scrolledWindowNew Nothing Nothing
                webView <- webViewNew
                window `containerAdd` scrollWin
                scrollWin `containerAdd` webView
                window `onDestroy` do
                    tryTakeMVar state
                    putMVar state Nothing
                    mainQuit
                    putMVar done ()
                jsContext <- webViewGetMainFrame webView >>= webFrameGetGlobalContext
                putMVar newState TestState {..}
--                widgetShowAll window
                mainGUI
            takeMVar newState
        Just s -> return s
    x <- postGUISync $ runReaderT ((f >>= valToText) `catch` \ (JSException e) -> valToText e) jsContext
    putMVar state $ Just TestState {..}
    return x

main = do
    runjs $ return ()
    Just TestState{..} <- takeMVar state
    postGUIAsync $ widgetDestroy window
    takeMVar done

test = runjs $ global ! "console" ! "log" # ["Hello"]

test2 = runjs $ eval "'Test'" >>= valToText
