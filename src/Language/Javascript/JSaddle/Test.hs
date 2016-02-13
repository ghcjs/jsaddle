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

module Language.Javascript.JSaddle.Test (
    testJSaddle
  , showJSaddle
  , listWindowProperties
) where

import Control.Applicative
import Prelude hiding((!!), catch)
import Graphics.UI.Gtk
       (Window, widgetDestroy, postGUIAsync, postGUISync, widgetShowAll,
        mainGUI, mainQuit, on, objectDestroy, containerAdd, scrolledWindowNew,
        windowSetPosition, windowSetDefaultSize, timeoutAddFull, windowNew,
        initGUI)
import Control.Concurrent
       (tryTakeMVar, forkIO, newMVar, putMVar, takeMVar, newEmptyMVar,
        yield)
import System.Glib.MainLoop (priorityLow)
import Graphics.UI.Gtk.General.Enums (WindowPosition(..))
import Graphics.UI.Gtk.WebKit.WebView
       (webViewGetMainFrame, webViewNew)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans.Reader (runReaderT)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.WebFrame
       (webFrameGetGlobalContext)
import Language.Javascript.JSaddle
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (forM, when)
import Control.Lens.Getter ((^.))
import Data.Monoid ((<>))
import Control.Concurrent.MVar (MVar)

data TestState = TestState { jsContext :: JSContextRef, window :: Window }

state :: MVar (Maybe TestState)
state = unsafePerformIO $ newMVar Nothing
{-# NOINLINE state #-}

done :: MVar ()
done = unsafePerformIO newEmptyMVar
{-# NOINLINE done #-}

-- >>> testJSaddle $ ((global ^. js "console" . js "log") # ["Hello"])
testJSaddle :: ToJSVal val => JSM val -> IO ()
testJSaddle = testJSaddle' False

-- >>> showJSaddle $ eval "document.body.innerHTML = 'Test'"
showJSaddle :: ToJSVal val => JSM val -> IO ()
showJSaddle = testJSaddle' True

debugLog :: String -> IO ()
debugLog = putStrLn

testJSaddle' :: ToJSVal val => Bool -> JSM val -> IO ()
testJSaddle' showWindow f = do
    debugLog "taking done"
    _ <- tryTakeMVar done
    debugLog "taking state"
    mbState <- takeMVar state
    TestState {..} <- case mbState of
        Nothing -> do
            debugLog "newState"
            newState <- newEmptyMVar
            debugLog "fork"
            _ <- forkIO $ do
                debugLog "initGUI"
                _ <- initGUI
                debugLog "windowNew"
                window <- windowNew
                debugLog "timeoutAdd"
                _ <- timeoutAddFull (yield >> return True) priorityLow 10
                windowSetDefaultSize window 900 600
                windowSetPosition window WinPosCenter
                scrollWin <- scrolledWindowNew Nothing Nothing
                webView <- webViewNew
                window `containerAdd` scrollWin
                scrollWin `containerAdd` webView
                _ <- on window objectDestroy $ do
                    debugLog "onDestroy"
                    _ <- tryTakeMVar state
                    debugLog "put state"
                    putMVar state Nothing
                    debugLog "mainQuit"
                    mainQuit
                    debugLog "put done"
                    putMVar done ()
                    return ()
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

listWindowProperties :: IO ()
listWindowProperties = testJSaddle $ T.pack . show <$> do
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
            return (n, T.pack " error " <> msg)









