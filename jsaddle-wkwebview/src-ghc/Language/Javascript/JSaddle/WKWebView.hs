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
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.StablePtr (StablePtr, newStablePtr)
import Language.Javascript.JSaddle (JSM)

foreign import ccall runInWKWebView :: StablePtr (WKWebView -> IO ()) -> CString -> StablePtr (CString -> IO ()) -> IO ()

-- | Run JSaddle in a WKWebView
run :: (CString -> IO ()) -> JSM () -> IO ()
run didRegisterForRemoteNotificationsWithDeviceToken f = do
    handler <- newStablePtr (jsaddleMain f)
    progName <- getProgName
    deviceTokenHandler <- newStablePtr didRegisterForRemoteNotificationsWithDeviceToken
    withCString progName $ \pn -> runInWKWebView handler pn deviceTokenHandler

-- | Run JSaddle in a WKWebView first loading the specified file
--   from the mainBundle (relative to the resourcePath).
runFile :: ByteString -- ^ The file to navigate to.
        -> ByteString -- ^ The path to allow read access to.
        -> (CString -> IO ())
        -> JSM ()
        -> IO ()
runFile url allowing didRegisterForRemoteNotificationsWithDeviceToken f = do
    handler <- newStablePtr (jsaddleMainFile url allowing f)
    progName <- getProgName
    deviceTokenHandler <- newStablePtr didRegisterForRemoteNotificationsWithDeviceToken
    withCString progName $ \pn -> runInWKWebView handler pn deviceTokenHandler

