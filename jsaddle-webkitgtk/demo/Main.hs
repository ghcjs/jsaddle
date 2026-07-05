{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- | Minimal jsaddle-webkitgtk demo: a click counter (exercises the
--   synchronous prompt bridge via the callback) and an uptime ticker
--   (exercises async batches from a background Haskell thread).
--
--   With @--smoke@ it also asserts a full round trip through the bridge
--   (async eval + result, and a Haskell callback invoked from JS) and
--   exits 0/1 — suitable for headless CI under @xvfb-run@.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Posix.Process (exitImmediately)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.WebKitGTK (run)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    smoke <- elem "--smoke" <$> getArgs
    -- exitImmediately: exitSuccess/exitFailure only throw in the calling
    -- thread, and everything here runs off the GTK main loop's thread.
    when smoke . void . forkIO $ do
        threadDelay 60000000
        putStrLn "SMOKE-TIMEOUT"
        exitImmediately (ExitFailure 1)
    run $ do
        doc <- jsg "document"
        body <- doc ! "body"
        void $ body # "insertAdjacentHTML" $
            ( "beforeend"
            , "<h1>jsaddle-webkitgtk demo</h1>\
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

        when smoke $ do
            v <- eval "6 * 7"
            n <- valToNumber v
            liftIO . putStrLn $ "SMOKE eval 6*7 = " ++ show n
            -- A Haskell callback invoked from an eval'd script goes through
            -- the synchronous JSaddleSync prompt path.
            got <- liftIO $ newIORef (0 :: Double)
            _ <- jsg "globalThis" <# "hsSmoke" $ fun $ \_ _ as -> case as of
                (a:_) -> valToNumber a >>= liftIO . writeIORef got
                _     -> return ()
            _ <- eval "globalThis.hsSmoke(123)"
            syncPoint
            liftIO $ do
                -- callbacks run on forked threads; give it a moment
                let waitCb 0 = readIORef got
                    waitCb k = do
                        x <- readIORef got
                        if x == 123 then return x
                                    else threadDelay 100000 >> waitCb (k - 1 :: Int)
                x <- waitCb 100
                putStrLn $ "SMOKE callback got " ++ show x
                if n == 42 && x == 123
                    then putStrLn "SMOKE-OK" >> exitImmediately ExitSuccess
                    else putStrLn "SMOKE-FAIL" >> exitImmediately (ExitFailure 1)
