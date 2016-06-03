{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
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
import GI.Gtk
       (Window, widgetDestroy, widgetShowAll,
        mainQuit, containerAdd, scrolledWindowNew,
        windowSetPosition, windowSetDefaultSize, windowNew)
import Control.Concurrent
       (tryTakeMVar, forkIO, newMVar, putMVar, takeMVar, newEmptyMVar,
        yield)
import GI.Gtk.Enums (WindowType(..), WindowPosition(..))
import GI.WebKit.Objects.WebView
       (webViewGetMainFrame, webViewNew)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans.Reader (runReaderT)
import GI.WebKit.Objects.WebFrame
       (webFrameGetGlobalContext)
import Language.Javascript.JSaddle
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (forM, when)
import Control.Lens.Getter ((^.))
import Data.Monoid ((<>))
import Control.Concurrent.MVar (MVar)
import qualified GI.Gtk.Functions as Gtk (init, main)
import GI.GLib.Functions (timeoutAdd)
import GI.Gtk.Enums (WindowPosition(..))
import Data.GI.Base.Signals (on)
import GI.GLib (idleAdd)
import GI.GLib.Constants(pattern PRIORITY_DEFAULT, pattern PRIORITY_LOW)
import GI.Gtk.Objects.Widget (onWidgetDestroy)
import GI.JavaScriptCore.Structs.GlobalContext (GlobalContext(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr)
import GI.Gtk.Objects.Adjustment (Adjustment(..))

-- | Post an action to be run in the main GUI thread.
--
-- The current thread blocks until the action completes and the result is
-- returned.
--
postGUISync :: IO a -> IO a
postGUISync action = do
  resultVar <- newEmptyMVar
  idleAdd PRIORITY_DEFAULT $ action >>= putMVar resultVar >> return False
  takeMVar resultVar

-- | Post an action to be run in the main GUI thread.
--
-- The current thread continues and does not wait for the result of the
-- action.
--
postGUIAsync :: IO () -> IO ()
postGUIAsync action = do
  idleAdd PRIORITY_DEFAULT $ action >> return False
  return ()

data TestState = TestState { jsContext :: GlobalContext, window :: Window }

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
debugLog _ = return ()

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
                _ <- Gtk.init mempty
                debugLog "windowNew"
                window <- windowNew WindowTypeToplevel
                debugLog "timeoutAdd"
                _ <- timeoutAdd PRIORITY_LOW 10 $ yield >> return True
                windowSetDefaultSize window 900 600
                windowSetPosition window WindowPositionCenter
                scrollWin <- scrolledWindowNew (Nothing :: Maybe Adjustment) (Nothing :: Maybe Adjustment)
                webView <- webViewNew
                window `containerAdd` scrollWin
                scrollWin `containerAdd` webView
                _ <- onWidgetDestroy window $ do
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
                Gtk.main
                debugLog "mainGUI exited"
            takeMVar newState
        Just s@TestState {..} -> do
            debugLog "maybe show (2)"
            when showWindow . postGUISync $ widgetShowAll window
            return s
    x <- postGUISync $ withContext jsContext $ runReaderT ((f >>= valToText >>= liftIO . putStrLn . T.unpack)
            `catch` \ (JSException e) -> valToText e >>= liftIO . putStrLn . T.unpack)
    debugLog "put state"
    putMVar state $ Just TestState {..}
    return x

withContext :: GlobalContext -> (JSContextRef -> IO a) -> IO a
withContext (GlobalContext fptr) f = withForeignPtr fptr $ f . castPtr

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









