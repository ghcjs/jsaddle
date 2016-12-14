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

