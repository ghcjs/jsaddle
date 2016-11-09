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
    ToJSVal(..)
  , ToJSString(..)
  , MakeObject(..)
) where

import Language.Javascript.JSaddle.Types
       (JSM, Object(..), JSString, JSVal)

-- | Anything that can be used to make a JavaScript value reference
class ToJSVal a where
    toJSVal :: a -> JSM JSVal

-- | Anything that can be used to make a JavaScript string reference
class ToJSVal a => ToJSString a where
    toJSString :: a -> JSString

-- | Anything that can be used to make a JavaScript object reference
class MakeObject this where
    makeObject :: this -> JSM Object

instance ToJSVal Object where
    toJSVal (Object r) = return r

-- | If we already have a Object we are fine
instance MakeObject Object where
    makeObject = return

