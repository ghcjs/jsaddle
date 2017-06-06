{-# LANGUAGE ForeignFunctionInterface #-}
module Language.Javascript.JSaddle.CLib.Internal where

import Foreign
import Foreign.C

#include "jsaddle.h"

data NativeCallbacks = NativeCallbacks
  { _nativeCallbacks_jsaddleStart :: !(FunPtr (IO ())) -- void (jsaddleStart *)()
  , _nativeCallbacks_jsaddleResult :: !(FunPtr (CString -> IO ())) -- void (jsaddleResult *)(char *)
  , _nativeCallbacks_jsaddleSyncResult :: !(FunPtr (CString -> IO CString)) -- char * (jsaddleSyncResult *)(char *)
  , _nativeCallbacks_jsaddleJsData :: !CString -- char * jsaddleJsData
  , _nativeCallbacks_jsaddleHtmlData :: !CString -- char * jsaddleHtmlData
  }

instance Storable NativeCallbacks where
  sizeOf _ = #{size native_callbacks}
  alignment _ = #{alignment native_callbacks}
  poke p nc = do
    #{poke native_callbacks, jsaddleStart} p $ _nativeCallbacks_jsaddleStart nc
    #{poke native_callbacks, jsaddleResult} p $ _nativeCallbacks_jsaddleResult nc
    #{poke native_callbacks, jsaddleSyncResult} p $ _nativeCallbacks_jsaddleSyncResult nc
    #{poke native_callbacks, jsaddleJsData} p $ _nativeCallbacks_jsaddleJsData nc
    #{poke native_callbacks, jsaddleHtmlData} p $ _nativeCallbacks_jsaddleHtmlData nc
  peek p = NativeCallbacks
    <$> #{peek native_callbacks, jsaddleStart} p
    <*> #{peek native_callbacks, jsaddleResult} p
    <*> #{peek native_callbacks, jsaddleSyncResult} p
    <*> #{peek native_callbacks, jsaddleJsData} p
    <*> #{peek native_callbacks, jsaddleHtmlData} p

data AppCallbacks = AppCallbacks
  { _appCallbacks_mainActivity_onCreate :: !(FunPtr (IO ()))
  , _appCallbacks_mainActivity_onStart :: !(FunPtr (IO ()))
  , _appCallbacks_mainActivity_onResume :: !(FunPtr (IO ()))
  , _appCallbacks_mainActivity_onPause :: !(FunPtr (IO ()))
  , _appCallbacks_mainActivity_onStop :: !(FunPtr (IO ()))
  , _appCallbacks_mainActivity_onDestroy :: !(FunPtr (IO ()))
  , _appCallbacks_mainActivity_onRestart :: !(FunPtr (IO ()))
  , _appCallbacks_mainActivity_onNewIntent :: !(FunPtr (CString -> CString -> IO ()))
  , _appCallbacks_firebaseInstanceIdService_sendRegistrationToServer :: !(FunPtr (CString -> IO ()))
  }

instance Storable AppCallbacks where
  sizeOf _ = #{size app_callbacks}
  alignment _ = #{alignment app_callbacks}
  poke p nc = do
    #{poke app_callbacks, mainActivity_onCreate} p $ _appCallbacks_mainActivity_onCreate nc
    #{poke app_callbacks, mainActivity_onStart} p $ _appCallbacks_mainActivity_onStart nc
    #{poke app_callbacks, mainActivity_onResume} p $ _appCallbacks_mainActivity_onResume nc
    #{poke app_callbacks, mainActivity_onPause} p $ _appCallbacks_mainActivity_onPause nc
    #{poke app_callbacks, mainActivity_onStop} p $ _appCallbacks_mainActivity_onStop nc
    #{poke app_callbacks, mainActivity_onDestroy} p $ _appCallbacks_mainActivity_onDestroy nc
    #{poke app_callbacks, mainActivity_onRestart} p $ _appCallbacks_mainActivity_onRestart nc
    #{poke app_callbacks, mainActivity_onNewIntent} p $ _appCallbacks_mainActivity_onNewIntent nc
    #{poke app_callbacks, firebaseInstanceIdService_sendRegistrationToServer} p $ _appCallbacks_firebaseInstanceIdService_sendRegistrationToServer nc
  peek p = AppCallbacks
    <$> #{peek app_callbacks, mainActivity_onCreate} p
    <*> #{peek app_callbacks, mainActivity_onStart} p
    <*> #{peek app_callbacks, mainActivity_onResume} p
    <*> #{peek app_callbacks, mainActivity_onPause} p
    <*> #{peek app_callbacks, mainActivity_onStop} p
    <*> #{peek app_callbacks, mainActivity_onDestroy} p
    <*> #{peek app_callbacks, mainActivity_onRestart} p
    <*> #{peek app_callbacks, mainActivity_onNewIntent} p
    <*> #{peek app_callbacks, firebaseInstanceIdService_sendRegistrationToServer} p
