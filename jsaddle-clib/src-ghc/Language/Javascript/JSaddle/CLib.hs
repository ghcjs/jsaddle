{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Language.Javascript.JSaddle.CLib
  ( jsaddleInit
  , NativeCallbacks (..)
  ) where

import Control.Monad (void)
import Control.Concurrent (forkIO)

import Data.Monoid ((<>))
import Data.ByteString (useAsCString, packCString)
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import Data.ByteString.Char8 (unpack)
import Data.Aeson (encode, decode)

import Foreign.C.String (CString, newCString)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Marshal.Utils (new)

import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (initState, runBatch, ghcjsHelpers)

import Language.Javascript.JSaddle.CLib.Internal

foreign import ccall safe "dynamic"
  mkCallback :: FunPtr (CString -> IO ()) -> CString -> IO ()

foreign import ccall safe "wrapper"
  wrapStartCallback :: IO () -> IO (FunPtr (IO ()))

foreign import ccall safe "wrapper"
  wrapMessageCallback :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

jsaddleInit :: JSM () -> FunPtr (CString -> IO ()) -> IO (Ptr NativeCallbacks)
jsaddleInit jsm evaluateJavascriptAsyncPtr = do
  let evaluateJavascriptAsync = mkCallback evaluateJavascriptAsyncPtr
  (processResult, start) <- runJavaScript (\batch ->
    useAsCString (toStrict $ "runJSaddleBatch(" <> encode batch <> ");") $
      evaluateJavascriptAsync) jsm
  jsaddleStartPtr <- wrapStartCallback $ void $ forkIO $ start
  jsaddleResultPtr <- wrapMessageCallback $ \s -> do
    result <- decode . fromStrict <$> packCString s
    case result of
      Nothing -> error $ "jsaddle message decode failed: " <> show result
      Just r -> processResult r
  jsaddleJsPtr <- newCString $ unpack $ toStrict jsaddleJs
  jsaddleHtmlPtr <- newCString $ unpack $ toStrict indexHtml
  new $ NativeCallbacks
    { _nativeCallbacks_jsaddleStart = jsaddleStartPtr
    , _nativeCallbacks_jsaddleResult = jsaddleResultPtr
    , _nativeCallbacks_jsaddleJsData = jsaddleJsPtr
    , _nativeCallbacks_jsaddleHtmlData = jsaddleHtmlPtr
    }

jsaddleJs :: ByteString
jsaddleJs = ghcjsHelpers <> "\
    \runJSaddleBatch = (function() {\n\
    \ " <> initState <> "\n\
    \ return function(batch) {\n\
    \ " <> runBatch (\a -> "jsaddle.postMessage(JSON.stringify(" <> a <> "));") <> "\
    \ };\n\
    \})();\n\
    \jsaddle.postReady();\n"

indexHtml :: ByteString
indexHtml =
    "<!DOCTYPE html>\n\
    \<html>\n\
    \<head>\n\
    \<title>JSaddle</title>\n\
    \</head>\n\
    \<body>\n\
    \</body>\n\
    \</html>"
