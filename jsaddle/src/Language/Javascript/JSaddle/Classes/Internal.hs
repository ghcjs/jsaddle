{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Classes.Internal
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

module Language.Javascript.JSaddle.Classes.Internal (
  -- * Type classes to convert Haskell data to JavaScript
    MakeObject(..)
  , MakeArgs(..)
) where

import Language.Javascript.JSaddle.Types
       (JSM, Object(..), JSVal)

-- | Anything that can be used to make a JavaScript object reference
class MakeObject this where
    makeObject :: this -> JSM Object

-- | If we already have a Object we are fine
instance MakeObject Object where
    makeObject = return

-- | Anything that can be used to make a list of JavaScript value
--   references for use as function arguments
class MakeArgs this where
    makeArgs :: this -> JSM [JSVal]

instance MakeArgs arg => MakeArgs (JSM arg) where
    makeArgs arg = arg >>= makeArgs

