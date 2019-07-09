module Language.Javascript.JSaddle.WKWebView
    ( jsaddleMain
    , jsaddleMainHTMLWithBaseURL
    , jsaddleMainFile
    , WKWebView(..)
    , run
    , run'
    , runWithAppConfig
    , runHTMLWithBaseURL
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
import Data.Word
import Foreign.Marshal.Utils (fromBool)
import Language.Javascript.JSaddle.WKWebView.Internal
       (jsaddleMain, jsaddleMainHTMLWithBaseURL, jsaddleMainFile, WKWebView(..), mainBundleResourcePath)
import System.Environment (getProgName)
import Foreign.C.String (CString, withCString)
import Foreign.StablePtr (StablePtr, newStablePtr)
import Language.Javascript.JSaddle (JSM)

foreign import ccall runInWKWebView
    :: StablePtr (WKWebView -> IO ())
    -> CString
    -> StablePtr (IO ()) -- willFinishLaunchingWithOptions
    -> StablePtr (IO ()) -- didFinishLaunchingWithOptions
    -> StablePtr (IO ()) -- applicationDidBecomeActive
    -> StablePtr (IO ()) -- applicationWillResignActive
    -> StablePtr (IO ()) -- applicationDidEnterBackground
    -> StablePtr (IO ()) -- applicationWillEnterForeground
    -> StablePtr (IO ()) -- applicationWillTerminate
    -> StablePtr (IO ()) -- applicationSignificantTimeChange
    -> StablePtr (CString -> IO ()) -- applicationUniversalLink
    -> Word64  -- whether to run requestAuthorizationWithOptions
    -> Word64 -- Ask for Badge authorization
    -> Word64 -- Ask for Sound authorization
    -> Word64 -- Ask for Alert authorization
    -> Word64 -- Ask for CarPlay authorization
    -> Word64 -- registerForRemoteNotifications
    -> StablePtr (CString -> IO ()) -- didRegisterForRemoteNotificationsWithDeviceToken
    -> StablePtr (CString -> IO ()) -- didFailToRegisterForRemoteNotificationsWithError
    -> Word64 -- Allow developer tools
    -> IO ()

data AppDelegateConfig = AppDelegateConfig
    { _appDelegateConfig_willFinishLaunchingWithOptions :: IO ()
    , _appDelegateConfig_didFinishLaunchingWithOptions :: IO ()
    , _appDelegateConfig_applicationDidBecomeActive :: IO ()
    , _appDelegateConfig_applicationWillResignActive :: IO ()
    , _appDelegateConfig_applicationDidEnterBackground :: IO ()
    , _appDelegateConfig_applicationWillEnterForeground :: IO ()
    , _appDelegateConfig_applicationWillTerminate :: IO ()
    , _appDelegateConfig_applicationSignificantTimeChange :: IO ()
    , _appDelegateConfig_applicationUniversalLink :: CString -> IO ()
    , _appDelegateConfig_appDelegateNotificationConfig :: AppDelegateNotificationConfig
    , _appDelegateConfig_developerExtrasEnabled :: Bool
    }

instance Default AppDelegateConfig where
    def = AppDelegateConfig
        { _appDelegateConfig_willFinishLaunchingWithOptions = return ()
        , _appDelegateConfig_didFinishLaunchingWithOptions = return ()
        , _appDelegateConfig_applicationDidBecomeActive = return ()
        , _appDelegateConfig_applicationWillResignActive = return ()
        , _appDelegateConfig_applicationDidEnterBackground = return ()
        , _appDelegateConfig_applicationWillEnterForeground = return ()
        , _appDelegateConfig_applicationWillTerminate = return ()
        , _appDelegateConfig_applicationSignificantTimeChange = return ()
        , _appDelegateConfig_applicationUniversalLink = \_ -> return ()
        , _appDelegateConfig_appDelegateNotificationConfig = def
        , _appDelegateConfig_developerExtrasEnabled = True
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
run :: JSM () -> IO ()
run f = run' def (jsaddleMain f)

-- | Run JSaddle in a WKWebView
runWithAppConfig :: AppDelegateConfig -> JSM () -> IO ()
runWithAppConfig cfg = run' cfg . jsaddleMain

-- | Run JSaddle in a WKWebView first loading the specified file
--   from the mainBundle (relative to the resourcePath).
runFile
  :: ByteString -- ^ The file to navigate to.
  -> ByteString -- ^ The path to allow read access to.
  -> AppDelegateConfig
  -> JSM ()
  -> IO ()
runFile url allowing cfg = run' cfg . jsaddleMainFile url allowing

-- | Run JSaddle in a WKWebView first loading the specified html
--   as though it came from the specified URL
--   (calls WKWebKit function loadHTMLString).
runHTMLWithBaseURL
  :: ByteString -- ^ HTML to load.
  -> ByteString -- ^ pretend it came from this URL.
  -> AppDelegateConfig
  -> JSM ()
  -> IO ()
runHTMLWithBaseURL url allowing cfg = run' cfg . jsaddleMainHTMLWithBaseURL url allowing

run' :: AppDelegateConfig
     -> (WKWebView -> IO ())
     -> IO ()
run' cfg main = do
    handler <- newStablePtr main
    progName <- getProgName

    -- AppDelegate callbacks
    willFinishLaunchingWithOptions <- newStablePtr $ _appDelegateConfig_willFinishLaunchingWithOptions cfg
    didFinishLaunchingWithOptions <- newStablePtr $ _appDelegateConfig_didFinishLaunchingWithOptions cfg
    applicationDidBecomeActive <- newStablePtr $ _appDelegateConfig_applicationDidBecomeActive cfg
    applicationWillResignActive <- newStablePtr $ _appDelegateConfig_applicationWillResignActive cfg
    applicationDidEnterBackground <- newStablePtr $ _appDelegateConfig_applicationDidEnterBackground cfg
    applicationWillEnterForeground <- newStablePtr $ _appDelegateConfig_applicationWillEnterForeground cfg
    applicationWillTerminate <- newStablePtr $ _appDelegateConfig_applicationWillTerminate cfg
    applicationSignificantTimeChange <- newStablePtr $ _appDelegateConfig_applicationSignificantTimeChange cfg
    applicationUniversalLink <- newStablePtr $ _appDelegateConfig_applicationUniversalLink cfg

    -- AppDelegate notification configuration
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
      willFinishLaunchingWithOptions
      didFinishLaunchingWithOptions
      applicationDidBecomeActive
      applicationWillResignActive
      applicationDidEnterBackground
      applicationWillEnterForeground
      applicationWillTerminate
      applicationSignificantTimeChange
      applicationUniversalLink
      (fromBool requestAuthorizationWithOptions)
      (fromBool $ Set.member AuthorizationOption_Badge authorizationOptions)
      (fromBool $ Set.member AuthorizationOption_Sound authorizationOptions)
      (fromBool $ Set.member AuthorizationOption_Alert authorizationOptions)
      (fromBool $ Set.member AuthorizationOption_CarPlay authorizationOptions)
      (fromBool registerForRemoteNotifications)
      didRegisterForRemoteNotificationsWithDeviceToken
      didFailToRegisterForRemoteNotificationsWithError
      (fromBool $ _appDelegateConfig_developerExtrasEnabled cfg)
