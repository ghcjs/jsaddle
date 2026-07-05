{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- | Minimal jsaddle-webview2 demo: a click counter (exercises the
--   synchronous prompt bridge via the callback) and an uptime ticker
--   (exercises async batches from a background Haskell thread).
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Text as T
import Language.Javascript.JSaddle
import qualified Language.Javascript.JSaddle.WebView2 as WV2

main :: IO ()
main = WV2.run $ do
    doc <- jsg "document"
    body <- doc ! "body"
    void $ body # "insertAdjacentHTML" $
        ( "beforeend"
        , "<h1>jsaddle-webview2 demo</h1>\
          \<p>Uptime: <span id=\"uptime\">0</span>s</p>\
          \<p><button id=\"btn\">Clicked <span id=\"count\">0</span> times</button></p>"
        )

    clicks <- liftIO $ newIORef (0 :: Int)
    btn <- doc # "getElementById" $ ["btn"]
    void $ btn # "addEventListener" $
        ( "click"
        , fun $ \_ _ _ -> do
            n <- liftIO $ atomicModifyIORef' clicks $ \c -> (c + 1, c + 1)
            el <- doc # "getElementById" $ ["count"]
            el <# "textContent" $ T.pack (show n)
        )

    seconds <- liftIO $ newIORef (0 :: Int)
    ctx <- askJSM
    void . liftIO . forkIO . forever $ do
        threadDelay 1000000
        s <- atomicModifyIORef' seconds $ \c -> (c + 1, c + 1)
        runJSaddle ctx $ do
            el <- jsg "document" # "getElementById" $ ["uptime"]
            el <# "textContent" $ T.pack (show s)
