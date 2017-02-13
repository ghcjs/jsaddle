module Language.Javascript.JSaddle.WKWebView
    ( jsaddleMain
    , jsaddleMainFile
    , WKWebView(..)
    , run
    , runFile
    , mainBundleResourcePath
    ) where

import Data.ByteString (ByteString)
import Language.Javascript.JSaddle.WKWebView.Internal (jsaddleMain, jsaddleMainFile, WKWebView(..), mainBundleResourcePath)
import System.Environment (getProgName)
import Foreign.C.String (CString, withCString)
import Foreign.StablePtr (StablePtr, newStablePtr)
import Language.Javascript.JSaddle (JSM)

foreign import ccall runInWKWebView :: StablePtr (WKWebView -> IO ()) -> CString -> IO ()

-- | Run JSaddle in a WKWebView
run :: JSM () -> IO ()
run f = do
    handler <- newStablePtr (jsaddleMain f)
    progName <- getProgName
    withCString progName $ runInWKWebView handler

-- | Run JSaddle in a WKWebView first loading the specified file
--   from the mainBundle (relative to the resourcePath).
runFile :: ByteString -- ^ The file to navigate to.
        -> ByteString -- ^ The path to allow read access to.
        -> JSM () -> IO ()
runFile url allowing f = do
    handler <- newStablePtr (jsaddleMainFile url allowing f)
    progName <- getProgName
    withCString progName $ runInWKWebView handler
