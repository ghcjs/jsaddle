{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
  , listWindowProperties
) where

import Control.Applicative
import Prelude hiding((!!), catch)
import Control.Monad.Trans.Reader (runReaderT)
import Language.Javascript.JSaddle
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (forever, forM)
import Control.Lens.Getter ((^.))
import Data.Monoid ((<>))
import Control.Concurrent.MVar
       (tryTakeMVar, putMVar, takeMVar, newEmptyMVar, MVar)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (threadDelay, forkIO, ThreadId)
import Data.Text (Text)
import Language.Javascript.JSaddle.Warp (run)

context :: MVar JSContextRef
context = unsafePerformIO newEmptyMVar
{-# NOINLINE context #-}

server :: MVar ThreadId
server = unsafePerformIO newEmptyMVar
{-# NOINLINE server #-}

startServer :: IO ()
startServer =
    tryTakeMVar server >>= maybe (forkIO $
        run 3709 $ do
            liftIO $ tryTakeMVar context
            liftIO . putMVar context =<< askJSM
            liftIO . forever $ threadDelay 1000000
        ) return >>= putMVar server

-- >>> testJSaddle $ ((global ^. js "console" . js "log") # ["Hello"])
testJSaddle :: ToJSVal val => JSM val -> IO ()
testJSaddle f = timeout 10000000 (do
    startServer
    c <- takeMVar context
    runReaderT (unJSM $ (f >>= valToText >>= liftIO . putStrLn . T.unpack)
        `catch` \ (JSException e) -> valToText e >>= liftIO . putStrLn . T.unpack) c
    putMVar context c) >>= maybe (putStrLn "testJSaddle timed out") return

debugLog :: String -> IO ()
debugLog _ = return ()

listWindowProperties :: IO ()
listWindowProperties = testJSaddle $ T.pack . show <$> do
  window <- jsg ("window" :: Text)
  names <- propertyNames window
  forM names $ \name -> do
        v <- window ^. js name >>= valToText
        return (strToText name, v)
    `catch`
        \(JSException e) -> do
            msg <- valToText e
            return (strToText name, T.pack " error " <> msg)









