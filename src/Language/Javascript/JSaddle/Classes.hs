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
    MakeVal(..)
  , MakeString(..)
  , MakeArgs(..)
  , MakeObject(..)
) where

import Control.Monad.IO.Class (MonadIO)
import Language.Javascript.JSaddle.Types
       (Object(..), JSString, JSVal(..))
import Language.Javascript.JSaddle.Monad (JSM)

-- | Anything that can be used to make a JavaScript value reference
class MakeVal a where
    makeVal :: a -> JSM JSVal

-- | Anything that can be used to make a JavaScript string reference
class MakeString a where
    makeString :: a -> JSString

-- | Anything that can be used to make a list of JavaScript value
--   references for use as function arguments
class MakeArgs this where
    makeArgs :: this -> JSM [JSVal]

-- | Anything that can be used to make a JavaScript object reference
class MakeObject this where
    makeObject :: this -> JSM Object

instance MakeVal Object where
    makeVal (Object r) = return r
    {-# INLINE makeVal #-}
