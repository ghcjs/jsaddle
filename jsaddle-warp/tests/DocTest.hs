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
import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid ((<>))
import System.Environment (getArgs)
import Language.Javascript.JSaddle.Run.Files (jsaddleJs)
import qualified Data.ByteString.Lazy.Char8 as BS (unpack)
import Test.WebDriver (runSession, defaultConfig, openPage, closeSession)

main :: IO ()
main = do
    putStrLn "Hello"
    jsaddlePath <- getArgs >>= \case
        [arg] -> return arg
        _ -> do
            putStrLn "Please give the path to the jsaddle package source"
            exitFailure
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    node <- system "phantomjs --version" >>= \case
                ExitSuccess -> return ()
                e           -> do
                    putStrLn "phantomjs not found"
                    exitWith e
    forkIO . void $ readProcess "phantomjs" ["--webdriver=4444"] (BS.unpack jsaddleJs) >>= putStr
    runSession defaultConfig $ do
        openPage "http://localhost:3709"
        liftIO $ doctest [
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
            "-package=deepseq-" ++ VERSION_deepseq,
            "-package=ghc-prim-" ++ VERSION_ghc_prim,
            "-i" <> "src",
            "-i" <> "src-ghc",
            "src/Language/Javascript/JSaddle/Test.hs",
            "-i" <> jsaddlePath,
            jsaddlePath </> "src-ghc/Data/JSString/Text.hs",
            jsaddlePath </> "src-ghc/Data/JSString/Internal/Type.hs",
            jsaddlePath </> "src-ghc/GHCJS/Foreign.hs",
            jsaddlePath </> "src-ghc/GHCJS/Foreign/Internal.hs",
            jsaddlePath </> "src-ghc/GHCJS/Internal/Types.hs",
            jsaddlePath </> "src-ghc/GHCJS/Marshal.hs",
            jsaddlePath </> "src-ghc/GHCJS/Marshal/Pure.hs",
            jsaddlePath </> "src-ghc/GHCJS/Marshal/Internal.hs",
            jsaddlePath </> "src-ghc/GHCJS/Prim.hs",
            jsaddlePath </> "src-ghc/GHCJS/Prim/Internal.hs",
            jsaddlePath </> "src-ghc/GHCJS/Types.hs",
            jsaddlePath </> "src-ghc/JavaScript/Array.hs",
            jsaddlePath </> "src-ghc/JavaScript/Array/Internal.hs",
            jsaddlePath </> "src-ghc/JavaScript/Object/Internal.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Arguments.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Classes.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Classes/Internal.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Evaluate.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Exception.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Monad.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Marshal/String.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Native.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Native/Internal.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Object.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Properties.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Run.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Run/Files.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/String.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Types.hs",
            jsaddlePath </> "src/Language/Javascript/JSaddle/Value.hs" ]
        closeSession
