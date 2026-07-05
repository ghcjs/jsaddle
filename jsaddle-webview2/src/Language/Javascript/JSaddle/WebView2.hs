{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
-- | Run jsaddle apps in a native Windows window hosting a Microsoft Edge
--   WebView2 control.  The Windows sibling of
--   "Language.Javascript.JSaddle.WKWebView" (macOS\/iOS) and
--   "Language.Javascript.JSaddle.WebKitGTK" (Linux).
--
--   Requirements on the target machine:
--
--   * the WebView2 runtime (preinstalled on Windows 11; Evergreen-installed
--     on most Windows 10 machines, or bundle the installer), and
--
--   * @WebView2Loader.dll@ from the WebView2 SDK NuGet package
--     (@Microsoft.Web.WebView2@, @runtimes\/win-x64\/native@) next to the
--     executable.  It is loaded dynamically, so nothing from the SDK is
--     needed at link time.
module Language.Javascript.JSaddle.WebView2
    ( run
    , runHTML
    , runURL
    , run'
    , WebView2Config(..)
    , WebView2(..)
    , webView2Hwnd
    ) where

import Control.Monad (unless)
import Data.ByteString (ByteString, useAsCString)
import Data.Default (Default(..))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Text.Encoding (encodeUtf8)

import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (StablePtr, newStablePtr)

import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.WebView2.Internal
       (jsaddleMain, jsaddleMainHTML, jsaddleMainURL, WebView2(..))
import System.Environment (getProgName)

foreign import ccall wv2GetHwnd :: WebView2 -> IO (Ptr ())

-- | The Win32 @HWND@ of the window hosting the WebView2, for native window
--   integration (menu bar, dialogs) by the embedding application.  Only
--   meaningful once the 'run'' callback has been entered.
webView2Hwnd :: WebView2 -> IO (Ptr ())
webView2Hwnd = wv2GetHwnd

foreign import ccall runJsaddleWebView2
    :: StablePtr (WebView2 -> IO ())
    -> CString -- ^ window title (utf8)
    -> CInt    -- ^ initial width  (<=0 for default)
    -> CInt    -- ^ initial height (<=0 for default)
    -> CInt    -- ^ enable dev tools
    -> IO CInt

data WebView2Config = WebView2Config
    { _webView2Config_title :: Maybe Text
    -- ^ Window title; 'Nothing' uses the program name.
    , _webView2Config_width :: Int
    -- ^ Initial window width; @<= 0@ for the system default.
    , _webView2Config_height :: Int
    -- ^ Initial window height; @<= 0@ for the system default.
    , _webView2Config_devTools :: Bool
    -- ^ Allow opening the Edge dev tools (F12).  Defaults to 'True'.
    }

instance Default WebView2Config where
    def = WebView2Config
        { _webView2Config_title = Nothing
        , _webView2Config_width = 0
        , _webView2Config_height = 0
        , _webView2Config_devTools = True
        }

-- | Run a jsaddle app in a WebView2 window (blank initial page).
run :: JSM () -> IO ()
run f = run' def (jsaddleMain f)

-- | Run a jsaddle app in a WebView2 window, starting from the given HTML
--   (loaded with @NavigateToString@: origin @about:blank@, 2MB limit).
runHTML :: ByteString -> WebView2Config -> JSM () -> IO ()
runHTML html cfg = run' cfg . jsaddleMainHTML html

-- | Run a jsaddle app in a WebView2 window, first navigating to the given
--   URL (@file:\/\/@, @https:\/\/@, ...).
runURL :: ByteString -> WebView2Config -> JSM () -> IO ()
runURL url cfg = run' cfg . jsaddleMainURL url

-- | Run an action with the WebView2 once its controller is ready.  This is
--   the escape hatch mirroring 'Language.Javascript.JSaddle.WKWebView.run'':
--   the callback runs on the UI thread before any navigation has started.
run' :: WebView2Config -> (WebView2 -> IO ()) -> IO ()
run' cfg main = do
    title <- maybe (T.pack <$> getProgName) pure (_webView2Config_title cfg)
    mainPtr <- newStablePtr main
    r <- useAsCString (encodeUtf8 title) $ \t ->
        runJsaddleWebView2 mainPtr t
            (fromIntegral $ _webView2Config_width cfg)
            (fromIntegral $ _webView2Config_height cfg)
            (if _webView2Config_devTools cfg then 1 else 0)
    unless (r == 0) . ioError . userError $
        "jsaddle-webview2 failed to start (is the WebView2 runtime installed and WebView2Loader.dll next to the executable?)"
