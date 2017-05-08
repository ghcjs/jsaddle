module Language.Javascript.JSaddle.WKWebView
    ( jsaddleMain
    , jsaddleMainFile
    , WKWebView(..)
    , run
    , runFile
    , mainBundleResourcePath
    ) where

import Data.ByteString (ByteString)
import Language.Javascript.JSaddle.WKWebView.Internal (jsaddleMain, jsaddleMainFile, WKWebView(..), mainBundleResourcePath, AppDelegateConfig (..), boolToCChar)
import System.Environment (getProgName)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types
import Foreign.StablePtr (StablePtr, newStablePtr)
import Language.Javascript.JSaddle (JSM)

foreign import ccall runInWKWebView :: StablePtr (WKWebView -> IO ())
                                    -> CString
                                    -> CChar  -- requestAuthorizationWithOptions
                                    -> CChar -- registerForRemoteNotifications
                                    -> StablePtr (CString -> IO ()) -- didRegisterForRemoteNotificationsWithDeviceToken
                                    -> IO ()

-- | Run JSaddle in a WKWebView
run :: (CString -> IO ()) -> JSM () -> IO ()
run didRegisterForRemoteNotificationsWithDeviceToken f = do
    handler <- newStablePtr (jsaddleMain f)
    progName <- getProgName
    deviceTokenHandler <- newStablePtr didRegisterForRemoteNotificationsWithDeviceToken
    withCString progName $ \pn -> runInWKWebView handler pn 1 1 deviceTokenHandler

-- | Run JSaddle in a WKWebView first loading the specified file
--   from the mainBundle (relative to the resourcePath).
runFile :: ByteString -- ^ The file to navigate to.
        -> ByteString -- ^ The path to allow read access to.
        -> AppDelegateConfig
        -> JSM ()
        -> IO ()
runFile url allowing cfg f = do
    handler <- newStablePtr (jsaddleMainFile url allowing f)
    progName <- getProgName
    let requestAuthorizationWithOptions = boolToCChar $ _appDelegateConfig_requestAuthorizationWithOptions cfg
        registerForRemoteNotifications = boolToCChar $ _appDelegateConfig_registerForRemoteNotifications cfg
    didRegisterForRemoteNotificationsWithDeviceToken <- newStablePtr $ _appDelegateConfig_didRegisterForRemoteNotificationsWithDeviceToken cfg
    withCString progName $ \pn -> runInWKWebView
      handler
      pn
      requestAuthorizationWithOptions
      registerForRemoteNotifications
      didRegisterForRemoteNotificationsWithDeviceToken

