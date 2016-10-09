{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Native
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Native (
#if !defined(ghcjs_HOST_OS)
    wrapJSVal
  , wrapJSString
  , withJSVal
  , withJSVals
  , withObject
  , withJSString
  , withToJSVal
#endif
) where

#if !defined(ghcjs_HOST_OS)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Language.Javascript.JSaddle.Types
       (JSContextRef(..), AsyncCommand(..), JSM(..), JSString(..),
        Object(..), JSVal(..), JSValueReceived(..), JSValueForSend(..),
        JSStringReceived(..), JSStringForSend(..), JSObjectForSend(..))
import Language.Javascript.JSaddle.Classes (ToJSVal(..))
import System.Mem.Weak (addFinalizer)
import Control.Monad.Primitive (touch)
import Control.Monad (when)

wrapJSVal :: JSValueReceived -> JSM JSVal
wrapJSVal (JSValueReceived ref) = do
    -- TODO make sure this ref has not already been wrapped (perhaps only in debug version)
    let result = JSVal ref
    when (ref >= 5) $ do
        ctx <- JSM ask
        liftIO . addFinalizer result $ doSendAsyncCommand ctx $ FreeRef $ JSValueForSend ref
    return result

wrapJSString :: MonadIO m => JSStringReceived -> m JSString
wrapJSString (JSStringReceived ref) = return $ JSString ref

withJSVal :: MonadIO m => JSVal -> (JSValueForSend -> m a) -> m a
withJSVal v@(JSVal ref) f =
 do result <- f (JSValueForSend ref)
    liftIO $ touch v
    return result

withJSVals :: MonadIO m => [JSVal] -> ([JSValueForSend] -> m a) -> m a
withJSVals v f =
 do result <- f (map (\(JSVal ref) -> JSValueForSend ref) v)
    liftIO $ mapM_ touch v
    return result

withObject :: MonadIO m => Object -> (JSObjectForSend -> m a) -> m a
withObject (Object o) f = withJSVal o (f . JSObjectForSend)

withJSString :: MonadIO m => JSString -> (JSStringForSend -> m a) -> m a
withJSString v@(JSString ref) f =
 do result <- f (JSStringForSend ref)
    liftIO $ touch v
    return result

withToJSVal :: ToJSVal val => val -> (JSValueForSend -> JSM a) -> JSM a
withToJSVal val f = do
    v <- toJSVal val
    withJSVal v f
#endif

