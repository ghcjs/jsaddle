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
    MakeValueRef(..)
  , MakeStringRef(..)
  , MakeArgRefs(..)
  , MakeObject(..)
) where

import Control.Monad.IO.Class (MonadIO)
import Language.Javascript.JSaddle.Types
       (Object(..), JSStringRef, JSValueRef)
import Language.Javascript.JSaddle.Monad (JSM)

-- | Anything that can be used to make a JavaScript value reference
class MakeValueRef a where
    makeValueRef :: a -> JSM JSValueRef

-- | Anything that can be used to make a JavaScript string reference
class MakeStringRef a where
    makeStringRef :: a -> JSStringRef

-- | Anything that can be used to make a list of JavaScript value
--   references for use as function arguments
class MakeArgRefs this where
    makeArgRefs :: this -> JSM [JSValueRef]

-- | Anything that can be used to make a JavaScript object reference
class MakeObject this where
    makeObject :: this -> JSM Object

instance MakeValueRef Object where
    makeValueRef (Object r) = return r
    {-# INLINE makeValueRef #-}
