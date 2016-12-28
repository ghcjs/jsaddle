module Language.Javascript.JSaddle.WKWebView
    ( jsaddleMain
    , WKWebView(..)
    , run
    ) where

import Language.Javascript.JSaddle.WKWebView.Internal (jsaddleMain, WKWebView(..))
import System.Environment (getProgName)
import Foreign.C.String (CString, withCString)
import Foreign.StablePtr (StablePtr, newStablePtr)
import Language.Javascript.JSaddle (JSM)

foreign import ccall runInWKWebView :: StablePtr (WKWebView -> IO ()) -> CString -> IO ()

run :: JSM () -> IO ()
run f = do
    handler <- newStablePtr (jsaddleMain f)
    progName <- getProgName
    withCString progName $ runInWKWebView handler
