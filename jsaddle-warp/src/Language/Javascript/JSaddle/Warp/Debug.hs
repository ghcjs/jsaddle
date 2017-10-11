{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Javascript.JSaddle.Warp.Debug
  ( debug
  , debugWrapper
  , refreshMiddleware
  ) where

import Control.Monad (when, void)
import Language.Javascript.JSaddle.Debug (addContext, removeContext)
import Language.Javascript.JSaddle.Monad
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, modifyMVar_, readMVar, putMVar, modifyMVar, tryPutMVar, takeMVar, tryTakeMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Exception (finally)
import Language.Javascript.JSaddle.WebSockets
import Language.Javascript.JSaddle.Types (JSM)
import Network.Wai.Handler.Warp (runSettings, setPort, setTimeout, defaultSettings)
import Network.Wai
       (Middleware, Response, ResponseReceived)
import qualified Network.Wai as W
       (responseLBS, requestMethod, pathInfo)
import qualified Network.HTTP.Types as H
       (status200)
import qualified Data.ByteString.Lazy as LBS
import Network.WebSockets (defaultConnectionOptions)
import Data.Monoid
import Foreign.Store (newStore, lookupStore, readStore)

-- | Start or restart the server.
-- To run this as part of every :reload use
-- > :def! reload (const $ return "::reload\nLanguage.Javascript.JSaddle.Warp.debug 3708 SomeMainModule.someMainFunction")
debug :: Int -> JSM () -> IO ()
debug port f = do
    debugWrapper $ \withRefresh registerContext ->
        runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
            jsaddleOr defaultConnectionOptions (registerContext >> f >> syncPoint) (withRefresh $ jsaddleAppWithJs $ jsaddleJs True)
    putStrLn $ "<a href=\"http://localhost:" <> show port <> "\">run</a>"

refreshMiddleware :: ((Response -> IO ResponseReceived) -> IO ResponseReceived) -> Middleware
refreshMiddleware refresh otherApp req sendResponse = case (W.requestMethod req, W.pathInfo req) of
    ("POST", "reload" : _) -> refresh sendResponse
    _ -> otherApp req sendResponse

debugWrapper :: (Middleware -> JSM () -> IO ()) -> IO ()
debugWrapper run = do
    reloadMVar <- newEmptyMVar
    reloadDoneMVars <- newMVar []
    contexts <- newMVar []
    let refresh sendResponse = do
          reloadDone <- newEmptyMVar
          modifyMVar_ reloadDoneMVars (return . (reloadDone:))
          readMVar reloadMVar
          r <- sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/json")] ("reload" :: LBS.ByteString)
          putMVar reloadDone ()
          return r
        start :: Int -> IO (IO Int)
        start expectedConnections = do
            serverDone <- newEmptyMVar
            ready <- newEmptyMVar
            let registerContext :: JSM ()
                registerContext = do
                    uuid <- askJSM
                    browsersConnected <- liftIO $ modifyMVar contexts (\ctxs -> return (uuid:ctxs, length ctxs + 1))
                    addContext
                    when (browsersConnected == expectedConnections) . void . liftIO $ tryPutMVar ready ()
            thread <- forkIO $
                finally (run (refreshMiddleware refresh) registerContext)
                    (putMVar serverDone ())
            _ <- forkIO $ threadDelay 10000000 >> void (tryPutMVar ready ())
            when (expectedConnections /= 0) $ takeMVar ready
            return $ do
                putMVar reloadMVar ()
                ctxs <- takeMVar contexts
                mapM_ removeContext ctxs
                takeMVar reloadDoneMVars >>= mapM_ takeMVar
                tryTakeMVar serverDone >>= \case
                    Nothing -> do
                        killThread thread
                        takeMVar serverDone
                    Just _ -> return ()
                return $ length ctxs
        restarter :: MVar (Int -> IO (IO Int)) -> IO Int -> IO ()
        restarter mvar stop = do
             start' <- takeMVar mvar
             n <- stop
             start' n >>= restarter mvar
    lookupStore shutdown_0 >>= \case
        Nothing -> do
            restartMVar <- newMVar start
            void . forkIO $ restarter restartMVar (return 0)
            void $ newStore restartMVar
        Just shutdownStore -> do
            restartMVar :: MVar (Int -> IO (IO Int)) <- readStore shutdownStore
            void $ tryTakeMVar restartMVar
            putMVar restartMVar start
  where shutdown_0 = 0
