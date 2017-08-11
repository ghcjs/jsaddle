-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Value
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------
module GHCJS.Foreign.Internal (
    jsTrue
  , jsFalse
  , jsNull
  , toJSBool
--  , fromJSBool
  , jsUndefined
  , isTruthy
  , isNull
  , isUndefined
  , JSType(..)
) where

import Language.Javascript.JSaddle.Types (JSVal(..), GHCJSPure(..))
import Language.Javascript.JSaddle.Native.Internal
       (valueToBool)
import Data.Typeable (Typeable)
import GHCJS.Prim (isNull, isUndefined)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef)

jsTrue :: JSVal
jsTrue = JSVal . unsafePerformIO $ newIORef 3
{-# NOINLINE jsTrue #-}

jsFalse :: JSVal
jsFalse = JSVal . unsafePerformIO $ newIORef 2
{-# NOINLINE jsFalse #-}

jsNull :: JSVal
jsNull = JSVal . unsafePerformIO $ newIORef 0
{-# NOINLINE jsNull #-}

toJSBool :: Bool -> JSVal
toJSBool b = JSVal . unsafePerformIO . newIORef $ if b then 3 else 2
{-# NOINLINE toJSBool #-}

jsUndefined :: JSVal
jsUndefined = JSVal . unsafePerformIO $ newIORef 1
{-# NOINLINE jsUndefined #-}

isTruthy :: JSVal -> GHCJSPure Bool
isTruthy = GHCJSPure . valueToBool
{-# INLINE isTruthy #-}

-- types returned by JS typeof operator
data JSType = Undefined
            | Object
            | Boolean
            | Number
            | String
            | Symbol
            | Function
            | Other    -- ^ implementation dependent
            deriving (Show, Eq, Ord, Enum, Typeable)

