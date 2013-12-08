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

import Prelude hiding((!!), catch)
import Graphics.UI.Gtk
       (Window, widgetDestroy, postGUIAsync, postGUISync, widgetShowAll,
        mainGUI, mainQuit, on, objectDestroy, containerAdd, scrolledWindowNew,
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
import Control.Monad (forM, when)
import System.Log.Logger (debugM)
import Control.Lens.Getter ((^.))
import Data.Monoid ((<>))

data TestState = TestState { jsContext :: JSContextRef, window :: Window }

state = unsafePerformIO $ newMVar Nothing
done = unsafePerformIO $ newEmptyMVar

-- >>> testJSC $ ((global ^. js "console" . js "log") # ["Hello"])
testJSC :: MakeValueRef val => JSC val -> IO ()
testJSC = testJSC' False

-- >>> showJSC $ eval "document.body.innerHTML = 'Test'"
showJSC :: MakeValueRef val => JSC val -> IO ()
showJSC = testJSC' True

debugLog = debugM "jsc"

testJSC' :: MakeValueRef val => Bool -> JSC val -> IO ()
testJSC' showWindow f = do
    debugLog "taking done"
    tryTakeMVar done
    debugLog "taking state"
    mbState <- takeMVar state
    TestState {..} <- case mbState of
        Nothing -> do
            debugLog "newState"
            newState <- newEmptyMVar
            debugLog "fork"
            forkIO $ do
                debugLog "initGUI"
                initGUI
                debugLog "windowNew"
                window <- windowNew
                debugLog "timeoutAdd"
                timeoutAddFull (yield >> return True) priorityHigh 10
                windowSetDefaultSize window 900 600
                windowSetPosition window WinPosCenter
                scrollWin <- scrolledWindowNew Nothing Nothing
                webView <- webViewNew
                window `containerAdd` scrollWin
                scrollWin `containerAdd` webView
                on window objectDestroy $ do
                    debugLog "onDestroy"
                    tryTakeMVar state
                    debugLog "put state"
                    putMVar state Nothing
                    debugLog "mainQuit"
                    mainQuit
                    debugLog "put done"
                    putMVar done ()
                    return True
                debugLog "get context"
                jsContext <- webViewGetMainFrame webView >>= webFrameGetGlobalContext
                debugLog "put initial state"
                putMVar newState TestState {..}
                debugLog "maybe show"
                when showWindow $ widgetShowAll window
                debugLog "mainGUI"
                mainGUI
                debugLog "mainGUI exited"
            takeMVar newState
        Just s@TestState {..} -> do
            debugLog "maybe show (2)"
            when showWindow . postGUISync $ widgetShowAll window
            return s
    x <- postGUISync $ runReaderT ((f >>= valToText >>= liftIO . putStrLn . T.unpack)
            `catch` \ (JSException e) -> valToText e >>= liftIO . putStrLn . T.unpack) jsContext
    debugLog "put state"
    putMVar state $ Just TestState {..}
    return x

main = do
    testJSC $ return ()
    Just TestState{..} <- takeMVar state
    postGUIAsync $ widgetDestroy window
    takeMVar done

listWindowProperties = testJSC $ T.pack . show <$> do
  window <- jsg "window"
  names <- propertyNames window
  forM names $ \name -> do
        v <- window ^. js name >>= valToText
        n <- strToText name
        return (n, v)
    `catch`
        \(JSException e) -> do
            n <- strToText name
            msg <- valToText e
            return (n, (T.pack $ " error ") <> msg)









