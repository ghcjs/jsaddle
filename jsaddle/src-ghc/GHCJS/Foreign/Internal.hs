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
import GHCJS.Prim

jsTrue :: JSVal
jsTrue = primToJSVal $ PrimVal_Bool True
{-# NOINLINE jsTrue #-}

jsFalse :: JSVal
jsFalse = primToJSVal $ PrimVal_Bool False
{-# NOINLINE jsFalse #-}

toJSBool :: Bool -> JSVal
toJSBool = primToJSVal . PrimVal_Bool
{-# NOINLINE toJSBool #-}

jsUndefined :: JSVal
jsUndefined = primToJSVal PrimVal_Null
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

