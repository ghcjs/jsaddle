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

foreign import ccall runInWKWebView
    :: StablePtr (WKWebView -> IO ())
    -> CString
    -> CChar  -- whether to run requestAuthorizationWithOptions
    -> CChar -- Ask for Badge authorization
    -> CChar -- Ask for Sound authorization
    -> CChar -- Ask for Alert authorization
    -> CChar -- Ask for CarPlay authorization
    -> CChar -- registerForRemoteNotifications
    -> StablePtr (CString -> IO ()) -- didRegisterForRemoteNotificationsWithDeviceToken
    -> StablePtr (CString -> IO ()) -- didFailToRegisterForRemoteNotificationsWithError
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
    , _appDelegateNotificationConfig_didFailToRegisterForRemoteNotificationsWithError :: CString -> IO ()
    }

instance Default AppDelegateNotificationConfig where
  def = AppDelegateNotificationConfig
    { _appDelegateNotificationConfig_requestAuthorizationWithOptions = Nothing
    , _appDelegateNotificationConfig_registerForRemoteNotifications = False
    , _appDelegateNotificationConfig_didRegisterForRemoteNotificationsWithDeviceToken = \_ -> return ()
    , _appDelegateNotificationConfig_didFailToRegisterForRemoteNotificationsWithError = \_ -> return ()
    }


-- | Run JSaddle in a WKWebView
run :: AppDelegateConfig -> JSM () -> IO ()
run = run' Nothing

-- | Run JSaddle in a WKWebView first loading the specified file
--   from the mainBundle (relative to the resourcePath).
runFile :: ByteString -- ^ The file to navigate to.
        -> ByteString -- ^ The path to allow read access to.
        -> AppDelegateConfig
        -> JSM ()
        -> IO ()
runFile url allowing = run' $ Just (url, allowing)

run' :: Maybe (ByteString, ByteString)
     -> AppDelegateConfig
     -> JSM ()
     -> IO ()
run' mUrl cfg f = do
    handler <- case mUrl of
      Just (url, allowing) -> newStablePtr (jsaddleMainFile url allowing f)
      Nothing -> newStablePtr (jsaddleMain f)
    progName <- getProgName
    let ncfg = _appDelegateConfig_appDelegateNotificationConfig cfg
        (requestAuthorizationWithOptions, authorizationOptions) =
          case _appDelegateNotificationConfig_requestAuthorizationWithOptions ncfg of
            Nothing -> (False, Set.empty)
            Just opts -> (True, opts)
        registerForRemoteNotifications = _appDelegateNotificationConfig_registerForRemoteNotifications ncfg
    didRegisterForRemoteNotificationsWithDeviceToken <- newStablePtr $ _appDelegateNotificationConfig_didRegisterForRemoteNotificationsWithDeviceToken ncfg
    didFailToRegisterForRemoteNotificationsWithError <- newStablePtr $ _appDelegateNotificationConfig_didFailToRegisterForRemoteNotificationsWithError ncfg
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
      didFailToRegisterForRemoteNotificationsWithError
