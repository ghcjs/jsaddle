module Language.Javascript.JSaddle.WKWebView
    ( jsaddleMain
    , jsaddleMainFile
    , WKWebView(..)
    , run
    , runFile
    , mainBundleResourcePath
    , AppDelegateConfig (..)
    , AppDelegateNotificationConfig (..)
    , AuthorizationOption (..)
    ) where

import Data.ByteString (ByteString)
import Data.Default (Default, def)
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Javascript.JSaddle.WKWebView.Internal (jsaddleMain, jsaddleMainFile, WKWebView(..), mainBundleResourcePath, boolToCChar)
import System.Environment (getProgName)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types
import Foreign.StablePtr (StablePtr, newStablePtr)
import Language.Javascript.JSaddle (JSM)

foreign import ccall runInWKWebView :: StablePtr (WKWebView -> IO ())
                                    -> CString
                                    -> CChar  -- whether to run requestAuthorizationWithOptions
                                    -> CChar -- Ask for Badge authorization
                                    -> CChar -- Ask for Sound authorization
                                    -> CChar -- Ask for Alert authorization
                                    -> CChar -- Ask for CarPlay authorization
                                    -> CChar -- registerForRemoteNotifications
                                    -> StablePtr (CString -> IO ()) -- didRegisterForRemoteNotificationsWithDeviceToken
                                    -> IO ()


data AppDelegateConfig = AppDelegateConfig
    { _appDelegateConfig_appDelegateNotificationConfig :: AppDelegateNotificationConfig
    }

instance Default AppDelegateConfig where
    def = AppDelegateConfig
        { _appDelegateConfig_appDelegateNotificationConfig = def
        }

data AuthorizationOption = AuthorizationOption_Badge
                         | AuthorizationOption_Sound
                         | AuthorizationOption_Alert
                         | AuthorizationOption_CarPlay
    deriving (Show, Read, Eq, Ord)

data AppDelegateNotificationConfig = AppDelegateNotificationConfig
    { _appDelegateNotificationConfig_requestAuthorizationWithOptions :: Maybe (Set AuthorizationOption)
    , _appDelegateNotificationConfig_registerForRemoteNotifications :: Bool
    , _appDelegateNotificationConfig_didRegisterForRemoteNotificationsWithDeviceToken :: CString -> IO ()
    }

instance Default AppDelegateNotificationConfig where
  def = AppDelegateNotificationConfig
    { _appDelegateNotificationConfig_requestAuthorizationWithOptions = Nothing
    , _appDelegateNotificationConfig_registerForRemoteNotifications = False
    , _appDelegateNotificationConfig_didRegisterForRemoteNotificationsWithDeviceToken = \_ -> return ()
    }


-- | Run JSaddle in a WKWebView
run :: (CString -> IO ()) -> JSM () -> IO ()
run didRegisterForRemoteNotificationsWithDeviceToken f = do
    handler <- newStablePtr (jsaddleMain f)
    progName <- getProgName
    deviceTokenHandler <- newStablePtr didRegisterForRemoteNotificationsWithDeviceToken
    withCString progName $ \pn -> runInWKWebView handler pn 1 1 1 1 1 1 deviceTokenHandler

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
    let ncfg = _appDelegateConfig_appDelegateNotificationConfig cfg
        (requestAuthorizationWithOptions, authorizationOptions) =
          case _appDelegateNotificationConfig_requestAuthorizationWithOptions ncfg of
            Nothing -> (False, Set.empty)
            Just opts -> (True, opts)
        registerForRemoteNotifications = _appDelegateNotificationConfig_registerForRemoteNotifications ncfg
    didRegisterForRemoteNotificationsWithDeviceToken <- newStablePtr $ _appDelegateNotificationConfig_didRegisterForRemoteNotificationsWithDeviceToken ncfg
    withCString progName $ \pn -> runInWKWebView
      handler
      pn
      (boolToCChar requestAuthorizationWithOptions)
      (boolToCChar $ Set.member AuthorizationOption_Badge authorizationOptions)
      (boolToCChar $ Set.member AuthorizationOption_Sound authorizationOptions)
      (boolToCChar $ Set.member AuthorizationOption_Alert authorizationOptions)
      (boolToCChar $ Set.member AuthorizationOption_CarPlay authorizationOptions)
      (boolToCChar registerForRemoteNotifications)
      didRegisterForRemoteNotificationsWithDeviceToken
