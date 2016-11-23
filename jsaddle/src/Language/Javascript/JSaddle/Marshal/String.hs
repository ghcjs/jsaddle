{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Marshal.String
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | `JSStrings` in JSaddle (when compiled with GHC) is not a `JSVal` instead it
--   is implemented with a `Text`.
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Marshal.String (
  -- * Type class to convert Haskell to JavaScript string
    ToJSString(..)
  , FromJSString(..)
) where

import Language.Javascript.JSaddle.Types (JSString)
import GHCJS.Marshal.Internal (ToJSVal(..), FromJSVal(..))

-- | Anything that can be used to make a JavaScript string
class ToJSVal a => ToJSString a where
    toJSString :: a -> JSString

-- | Anything that can be constructed from a JavaScript string
class FromJSVal a => FromJSString a where
    fromJSString :: JSString -> a

