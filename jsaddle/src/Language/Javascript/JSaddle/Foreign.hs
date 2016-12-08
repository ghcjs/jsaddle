{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Value
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | This module is like `GHCJS.Foreign` but some functions are in
--
-----------------------------------------------------------------------------
module Language.Javascript.JSaddle.Foreign (
    jsTrue
  , jsFalse
  , jsNull
  , toJSBool
--  , fromJSBoolIO
  , jsUndefined
  , isTruthyIO
  , isNullIO
  , isUndefinedIO
--  , isObjectIO
--  , isFunctionIO
--  , isStringIO
--  , isBooleanIO
--  , isSymbolIO
--  , isNumberIO
) where

#ifdef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Types (JSM, JSVal)
import GHCJS.Foreign (toJSBool, isTruthy, jsNull, jsUndefined, jsTrue, jsFalse, isNull, isUndefined)
#else
import Language.Javascript.JSaddle.Types (JSM, JSVal(..))
import Language.Javascript.JSaddle.Native.Internal
       (withJSVal)
import Language.Javascript.JSaddle.Run
       (Command(..), Result(..), sendCommand)
#endif

#ifndef ghcjs_HOST_OS
jsTrue :: JSVal
jsTrue = JSVal 3
{-# INLINE jsTrue #-}

jsFalse :: JSVal
jsFalse = JSVal 2
{-# INLINE jsFalse #-}

jsNull :: JSVal
jsNull = JSVal 0
{-# INLINE jsNull #-}

toJSBool :: Bool -> JSVal
toJSBool b = JSVal $ if b then 3 else 2
{-# INLINE toJSBool #-}

jsUndefined :: JSVal
jsUndefined = JSVal 1
{-# INLINE jsUndefined #-}
#endif

isTruthyIO :: JSVal -> JSM Bool
#ifdef ghcjs_HOST_OS
isTruthyIO = return . isTruthy
#else
isTruthyIO (JSVal 0) = return False -- null
isTruthyIO (JSVal 1) = return False -- undefined
isTruthyIO (JSVal 2) = return False -- false
isTruthyIO (JSVal 3) = return True  -- true
isTruthyIO v = withJSVal v $ \rval -> do
                ~(ValueToBoolResult result) <- sendCommand (ValueToBool rval)
                return result
#endif
{-# INLINE isTruthyIO #-}

isNullIO :: JSVal -> JSM Bool
#ifdef ghcjs_HOST_OS
isNullIO = return . isNull
#else
isNullIO (JSVal 0) = return True
isNullIO v = withJSVal v $ \rval -> do
                ~(IsNullResult result) <- sendCommand $ IsNull rval
                return result
#endif
{-# INLINE isNullIO #-}

isUndefinedIO :: JSVal -> JSM Bool
#ifdef ghcjs_HOST_OS
isUndefinedIO = return . isUndefined
#else
isUndefinedIO (JSVal 1) = return True
isUndefinedIO v = withJSVal v $ \rval -> do
            ~(IsUndefinedResult result) <- sendCommand $ IsUndefined rval
            return result
#endif
{-# INLINE isUndefinedIO #-}

