{-# LANGUAGE CPP #-}
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
module GHCJS.Foreign (
    jsTrue
  , jsFalse
  , jsNull
  , toJSBool
--  , fromJSBool
  , jsUndefined
  , isTruthy
  , isNull
  , isUndefined
  , isObject
  , isFunction
  , isString
  , isBoolean
  , isSymbol
  , isNumber
  , JSType(..)
  , jsTypeOf
) where

import GHCJS.Foreign.Internal
import Language.Javascript.JSaddle.Types (JSVal(..), GHCJSPure(..))
import Language.Javascript.JSaddle.Object (jsg1)
import GHCJS.Marshal (FromJSVal(..))

isObject :: JSVal -> GHCJSPure Bool
isObject v = GHCJSPure $ jsg1 "h$isObject" v >>= fromJSValUnchecked
{-# INLINE isObject #-}

isFunction :: JSVal -> GHCJSPure Bool
isFunction v = GHCJSPure $ jsg1 "h$isFunction" v >>= fromJSValUnchecked
{-# INLINE isFunction #-}

isString :: JSVal -> GHCJSPure Bool
isString v = GHCJSPure $ jsg1 "h$isString" v >>= fromJSValUnchecked
{-# INLINE isString #-}

isBoolean :: JSVal -> GHCJSPure Bool
isBoolean v = GHCJSPure $ jsg1 "h$isBoolean" v >>= fromJSValUnchecked
{-# INLINE isBoolean #-}

isSymbol :: JSVal -> GHCJSPure Bool
isSymbol v = GHCJSPure $ jsg1 "h$isSymbol" v >>= fromJSValUnchecked
{-# INLINE isSymbol #-}

isNumber :: JSVal -> GHCJSPure Bool
isNumber v = GHCJSPure $ jsg1 "h$isNumber" v >>= fromJSValUnchecked
{-# INLINE isNumber #-}

jsTypeOf :: JSVal -> GHCJSPure JSType
jsTypeOf v = GHCJSPure $ toEnum <$> (jsg1 "h$jsonTypeOf" v >>= fromJSValUnchecked)
{-# INLINE jsTypeOf #-}
