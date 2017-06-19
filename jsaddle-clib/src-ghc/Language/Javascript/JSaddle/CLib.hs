{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Language.Javascript.JSaddle.CLib
  ( jsaddleInit
  , NativeCallbacks (..)
  , AppCallbacks (..)
  , AppConfig (..)
  , pokeAppConfig
  , appConfigToAppCallbacks
  ) where

import Control.Monad (void)
import Control.Concurrent (forkIO)

import Data.Aeson (encode, decode)
import Data.ByteString (useAsCString, packCString)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict)
import Data.Default (def, Default)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Encoding as T

import Foreign.C.String (CString, newCString)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (poke)
import Foreign.Marshal.Utils (new)

import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Language.Javascript.JSaddle.Run.Files (initState, runBatch, ghcjsHelpers)

import Language.Javascript.JSaddle.CLib.Internal

foreign import ccall safe "wrapper"
  wrapStartCallback :: IO () -> IO (FunPtr (IO ()))

foreign import ccall safe "wrapper"
  wrapMessageCallback :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

foreign import ccall safe "wrapper"
  wrapMessageCallback2 :: (CString -> CString -> IO ()) -> IO (FunPtr (CString -> CString -> IO ()))

foreign import ccall safe "wrapper"
  wrapSyncCallback :: (CString -> IO CString) -> IO (FunPtr (CString -> IO CString))

jsaddleInit :: JSM () -> (CString -> IO ()) -> IO (Ptr NativeCallbacks)
jsaddleInit jsm evaluateJavascriptAsync = do
  (processResult, processSyncResult, start) <- runJavaScript (\batch ->
    useAsCString (toStrict $ "runJSaddleBatch(" <> encode batch <> ");")
      evaluateJavascriptAsync) jsm
  jsaddleStartPtr <- wrapStartCallback $ void $ forkIO start
  jsaddleResultPtr <- wrapMessageCallback $ \s -> do
    result <- decode . fromStrict <$> packCString s
    case result of
      Nothing -> error $ "jsaddle message decode failed: " <> show result
      Just r -> processResult r
  jsaddleSyncResultPtr <- wrapSyncCallback $ \s -> do
    result <- decode . fromStrict <$> packCString s
    case result of
      Nothing -> error $ "jsaddle message decode failed: " <> show result
      Just r -> newCString =<< unpack . toStrict . encode <$> processSyncResult r
  jsaddleJsPtr <- newCString $ unpack $ toStrict jsaddleJs
  jsaddleHtmlPtr <- newCString $ unpack $ toStrict indexHtml
  new NativeCallbacks
    { _nativeCallbacks_jsaddleStart = jsaddleStartPtr
    , _nativeCallbacks_jsaddleResult = jsaddleResultPtr
    , _nativeCallbacks_jsaddleSyncResult = jsaddleSyncResultPtr
    , _nativeCallbacks_jsaddleJsData = jsaddleJsPtr
    , _nativeCallbacks_jsaddleHtmlData = jsaddleHtmlPtr
    }

data AppConfig = AppConfig
  { _appConfig_mainActivityOnCreate :: IO ()
  , _appConfig_mainActivityOnStart :: IO ()
  , _appConfig_mainActivityOnResume :: IO ()
  , _appConfig_mainActivityOnPause :: IO ()
  , _appConfig_mainActivityOnStop :: IO ()
  , _appConfig_mainActivityOnDestroy :: IO ()
  , _appConfig_mainActivityOnRestart :: IO ()
  , _appConfig_mainActivityOnNewIntent :: (Text -> Text -> IO ())
  , _appConfig_firebaseInstanceIdServiceSendRegistrationToServer :: Text -> IO ()
  }

instance Default AppConfig where
  def = AppConfig
    { _appConfig_mainActivityOnCreate = return ()
    , _appConfig_mainActivityOnStart = return ()
    , _appConfig_mainActivityOnResume = return ()
    , _appConfig_mainActivityOnPause = return ()
    , _appConfig_mainActivityOnStop = return ()
    , _appConfig_mainActivityOnDestroy = return ()
    , _appConfig_mainActivityOnRestart = return ()
    , _appConfig_mainActivityOnNewIntent = \_ _ -> return ()
    , _appConfig_firebaseInstanceIdServiceSendRegistrationToServer = \_ -> return ()
    }

appConfigToAppCallbacks :: AppConfig -> IO AppCallbacks
appConfigToAppCallbacks c = do
  create <- wrapStartCallback $ _appConfig_mainActivityOnCreate c
  start <- wrapStartCallback $ _appConfig_mainActivityOnStart c
  resume <- wrapStartCallback $ _appConfig_mainActivityOnResume c
  pause <- wrapStartCallback $ _appConfig_mainActivityOnPause c
  stop <- wrapStartCallback $ _appConfig_mainActivityOnStop c
  destroy <- wrapStartCallback $ _appConfig_mainActivityOnDestroy c
  restart <- wrapStartCallback $ _appConfig_mainActivityOnRestart c
  newIntent <- wrapMessageCallback2 $ \intentAction intentData -> do
    intentAction' <- fromUtf8CString intentAction
    intentData' <- fromUtf8CString intentData
    _appConfig_mainActivityOnNewIntent c intentAction' intentData'
  firebaseRegPtr <- wrapMessageCallback $ \token -> do
    token' <- fromUtf8CString token
    _appConfig_firebaseInstanceIdServiceSendRegistrationToServer c token'
  return $ AppCallbacks
    { _appCallbacks_mainActivity_onCreate = create
    , _appCallbacks_mainActivity_onStart = start
    , _appCallbacks_mainActivity_onResume = resume
    , _appCallbacks_mainActivity_onPause = pause
    , _appCallbacks_mainActivity_onStop = stop
    , _appCallbacks_mainActivity_onDestroy = destroy
    , _appCallbacks_mainActivity_onRestart = restart
    , _appCallbacks_mainActivity_onNewIntent = newIntent
    , _appCallbacks_firebaseInstanceIdService_sendRegistrationToServer = firebaseRegPtr
    }

fromUtf8CString :: CString -> IO Text
fromUtf8CString = fmap T.decodeUtf8 . packCString

pokeAppConfig :: Ptr AppCallbacks -> AppConfig -> IO ()
pokeAppConfig ptr cfg = poke ptr =<< appConfigToAppCallbacks cfg

jsaddleJs :: ByteString
jsaddleJs = ghcjsHelpers <> "\
    \runJSaddleBatch = (function() {\n\
    \ " <> initState <> "\n\
    \ return function(batch) {\n\
    \ " <> runBatch (\a -> "jsaddle.postMessage(JSON.stringify(" <> a <> "));")
              (Just (\a -> "JSON.parse(jsaddle.syncMessage(JSON.stringify(" <> a <> ")))")) <> "\
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
