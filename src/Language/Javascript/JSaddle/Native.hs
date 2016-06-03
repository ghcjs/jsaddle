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
    makeNewJSVal
  , makeNewJSString
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
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSObjectRef, OpaqueJSString, JSStringRef, OpaqueJSContext,
        OpaqueJSValue, JSValueRef)
import Foreign.ForeignPtr
       (newForeignPtr_, touchForeignPtr, FinalizerPtr, newForeignPtr,
        FinalizerEnvPtr, newForeignPtrEnv, ForeignPtr)
import Control.Monad.IO.Class (MonadIO, MonadIO(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
       (jsvalueprotect)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
       (jsstringretain)
import Language.Javascript.JSaddle.Types
       (JSM, JSString, Object(..), JSVal)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Language.Javascript.JSaddle.Classes (ToJSVal(..), ToJSVal)
import Foreign.Ptr (nullPtr)

makeNewJSVal :: JSValueRef -> JSM (ForeignPtr OpaqueJSValue)
makeNewJSVal val = do
    ctx <- ask
    liftIO $ do
        jsvalueprotect ctx val
        newForeignPtrEnv jsValueUnprotect ctx val

foreign import ccall unsafe "&JSValueUnprotect"
  jsValueUnprotect :: FinalizerEnvPtr OpaqueJSContext OpaqueJSValue

makeNewJSString :: MonadIO m => JSStringRef -> m (ForeignPtr OpaqueJSString)
makeNewJSString s | s == nullPtr =
    liftIO $ newForeignPtr_ s
makeNewJSString s =
    liftIO $ do
        s' <- jsstringretain s
        newForeignPtr jsStringRelease s'

wrapJSString :: MonadIO m => JSStringRef -> m (ForeignPtr OpaqueJSString)
wrapJSString s | s == nullPtr =
    liftIO $ newForeignPtr_ s
wrapJSString s = liftIO $ newForeignPtr jsStringRelease s

foreign import ccall unsafe "&JSStringRelease"
  jsStringRelease :: FinalizerPtr OpaqueJSString

withJSVal :: MonadIO m => JSVal -> (JSValueRef -> m a) -> m a
withJSVal v f =
 do result <- f (unsafeForeignPtrToPtr v)
    liftIO $ touchForeignPtr v
    return result

withJSVals :: MonadIO m => [JSVal] -> ([JSValueRef] -> m a) -> m a
withJSVals v f =
 do result <- f (map unsafeForeignPtrToPtr v)
    liftIO $ mapM_ touchForeignPtr v
    return result

withObject :: MonadIO m => Object -> (JSObjectRef -> m a) -> m a
withObject (Object o) f = do
    result <- f (unsafeForeignPtrToPtr o)
    liftIO $ touchForeignPtr o
    return result

withJSString :: MonadIO m => JSString -> (JSStringRef -> m a) -> m a
withJSString v f =
 do result <- f (unsafeForeignPtrToPtr v)
    liftIO $ touchForeignPtr v
    return result

withToJSVal :: ToJSVal val => val -> (JSValueRef -> JSM a) -> JSM a
withToJSVal val f = do
    v <- toJSVal val
    withJSVal v f
#endif

