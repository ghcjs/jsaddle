{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Classes
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | These classes are used to make various JavaScript types
--   out of whatever we have.  Functions in jsaddle take these as inputs.
--   This alows implicit casting and eager evaluation.
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Classes (
  -- * Type classes to convert Haskell data to JavaScript
    PToJSVal(..)
  , PFromJSVal(..)
  , ToJSVal(..)
  , FromJSVal(..)
  , ToJSString(..)
  , FromJSString(..)
  , MakeObject(..)
  , MakeArgs(..)
) where

#ifdef ghcjs_HOST_OS
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
#else
import GHCJS.Marshal.Internal (ToJSVal(..), FromJSVal(..))
#endif
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import Language.Javascript.JSaddle.Classes.Internal (MakeObject(..), MakeArgs(..))
import Language.Javascript.JSaddle.Marshal.String (ToJSString(..), FromJSString(..))
