{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Javascript.JSaddle.CLib.Internal where

import Foreign
import Foreign.C

#include "jsaddle.h"

data NativeCallbacks = NativeCallbacks
  { _nativeCallbacks_jsaddleStart :: !(FunPtr (IO ())) -- void (jsaddleStart *)()
  , _nativeCallbacks_jsaddleResult :: !(FunPtr (CString -> IO ())) -- void (jsaddleResult *)(char *)
  , _nativeCallbacks_jsaddleJsData :: !CString -- char * jsaddleJsData
  , _nativeCallbacks_jsaddleHtmlData :: !CString -- char * jsaddleHtmlData
  }

instance Storable NativeCallbacks where
  sizeOf _ = #{size native_callbacks}
  alignment _ = #{alignment native_callbacks}
  poke p nc = do
    #{poke native_callbacks, jsaddleStart} p $ _nativeCallbacks_jsaddleStart nc
    #{poke native_callbacks, jsaddleResult} p $ _nativeCallbacks_jsaddleResult nc
    #{poke native_callbacks, jsaddleJsData} p $ _nativeCallbacks_jsaddleJsData nc
    #{poke native_callbacks, jsaddleHtmlData} p $ _nativeCallbacks_jsaddleHtmlData nc
  peek p = NativeCallbacks
    <$> #{peek native_callbacks, jsaddleStart} p
    <*> #{peek native_callbacks, jsaddleResult} p
    <*> #{peek native_callbacks, jsaddleJsData} p
    <*> #{peek native_callbacks, jsaddleHtmlData} p

data AppCallbacks = AppCallbacks
  { _appCallbacks_firebaseInstanceIdService_sendRegistrationToServer :: !(FunPtr (CString -> IO ()))
  }

instance Storable AppCallbacks where
  sizeOf _ = #{size app_callbacks}
  alignment _ = #{alignment app_callbacks}
  poke p nc = do
    #{poke app_callbacks, firebaseInstanceIdService_sendRegistrationToServer} p $ _appCallbacks_firebaseInstanceIdService_sendRegistrationToServer nc
  peek p = AppCallbacks
    <$> #{peek app_callbacks, firebaseInstanceIdService_sendRegistrationToServer} p
