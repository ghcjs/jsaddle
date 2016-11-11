{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
module Main (
    main
) where

import Test.DocTest
import System.IO (stderr, BufferMode(..), stdout, hSetBuffering)
import System.FilePath ((</>))
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Process (readProcess, system)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Monoid ((<>))
import System.Environment (getArgs)
import Language.Javascript.JSaddle.Run.Files (jsaddleJs)
import qualified Data.ByteString.Lazy.Char8 as BS (unpack)

main :: IO ()
main = do
    jsaddlePath <- getArgs >>= \case
        [arg] -> return arg
        _ -> do
            putStrLn "Please give the path to the jsaddle package source"
            exitFailure
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    node <- system "nodejs --version" >>= \case
                ExitSuccess -> return "nodejs"
                _           -> return "node"
    system (node <> " --version") >>= \case
                ExitSuccess -> putStrLn "Node.js found"
                e           -> do
                    putStrLn "Node.js not found"
                    exitWith e
    forkIO . void $ readProcess node ["--harmony"] (BS.unpack jsaddleJs) >>= putStr
    doctest [
        "-hide-all-packages",
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
        "-package=warp-" ++ VERSION_warp,
        "-package=http-types-" ++ VERSION_http_types,
        "-package=stm-" ++ VERSION_stm,
        "-package=time-" ++ VERSION_time,
        "-package=containers-" ++ VERSION_containers,
        "-package=process-" ++ VERSION_process,
        "-package=filepath-" ++ VERSION_filepath,
        "-package=ref-tf-" ++ VERSION_ref_tf,
        "-i" <> jsaddlePath </> "jsaddle-warp/src",
        jsaddlePath </> "jsaddle-warp/src/Language/Javascript/JSaddle/Test.hs",
        "-i" <> jsaddlePath </> "jsaddle",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle.hs",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle/Arguments.hs",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle/Classes.hs",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle/Evaluate.hs",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle/Exception.hs",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle/Monad.hs",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle/Native.hs",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle/Object.hs",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle/Properties.hs",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle/Run.hs",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle/Run/Files.hs",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle/String.hs",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle/Types.hs",
        jsaddlePath </> "jsaddle/src/Language/Javascript/JSaddle/Value.hs" ]
