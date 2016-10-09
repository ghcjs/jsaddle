{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
module Main (
    main
) where

#ifndef ghcjs_HOST_OS
import Test.DocTest
import System.IO (stderr, BufferMode(..), stdout, hSetBuffering)
import System.FilePath ((</>))
import System.Exit (exitWith, ExitCode(..))
import System.Process (system)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Monoid ((<>))
import Paths_jsaddle (getDataDir)
#endif

main :: IO ()
main = do
#ifdef ghcjs_HOST_OS
    putStrLn "TODO find a way to run doctest tests with GHCJS"
#else
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    dataDir <- getDataDir
    node <- system "nodejs --version" >>= \case
                ExitSuccess -> return "nodejs"
                _           -> return "node"
    system (node <> " --version") >>= \case
                ExitSuccess -> putStrLn "Node.js found"
                e           -> do
                    putStrLn "Node.js not found"
                    exitWith e
    forkIO . void . system $ node <> " --harmony " <> dataDir </> "data/jsaddle.js"
    doctest [
        "-hide-all-packages",
        "-package=template-haskell-" ++ VERSION_template_haskell,
        "-package=base-" ++ VERSION_base,
        "-package=lens-" ++ VERSION_lens,
        "-package=text-" ++ VERSION_text,
        "-package=bytestring-" ++ VERSION_bytestring,
        "-package=transformers-" ++ VERSION_transformers,
        "-package=websockets-" ++ VERSION_websockets,
        "-package=primitive-" ++ VERSION_primitive,
        "-package=aeson-" ++ VERSION_aeson,
        "-package=websockets-" ++ VERSION_websockets,
        "-package=wai-" ++ VERSION_wai,
        "-package=wai-websockets-" ++ VERSION_wai_websockets,
        "-package=wai-app-static-" ++ VERSION_wai_app_static,
        "-package=warp-" ++ VERSION_warp,
        "-package=http-types-" ++ VERSION_http_types,
        "-package=stm-" ++ VERSION_stm,
        "-package=time-" ++ VERSION_time,
        "-package=containers-" ++ VERSION_containers,
        "-package=process-" ++ VERSION_process,
        "-package=filepath-" ++ VERSION_filepath,
        "-isrc",
        "src/Language/Javascript/JSaddle/Arguments.hs",
        "src/Language/Javascript/JSaddle/Classes.hs",
        "src/Language/Javascript/JSaddle/Evaluate.hs",
        "src/Language/Javascript/JSaddle/Exception.hs",
        "src/Language/Javascript/JSaddle/Monad.hs",
        "src/Language/Javascript/JSaddle/Native.hs",
        "src/Language/Javascript/JSaddle/Object.hs",
        "src/Language/Javascript/JSaddle/Properties.hs",
        "src/Language/Javascript/JSaddle/String.hs",
        "src/Language/Javascript/JSaddle/Test.hs",
        "src/Language/Javascript/JSaddle/Types.hs",
        "src/Language/Javascript/JSaddle/Value.hs" ]
#endif
