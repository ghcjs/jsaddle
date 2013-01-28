-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC.Classes
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | These classes are used to make various JavaScriptCore types
--   out of whatever we have.  Functions in jsc take these as inputs.
--   This alows implicit casting and eager evaluation.
--
-----------------------------------------------------------------------------

module Language.Javascript.JSC.Classes (
  -- * Type classes to convert Haskell data to JavaScript
    MakeValueRef(..)
  , MakeStringRef(..)
  , MakeArgRefs(..)
  , MakeObjectRef(..)
  , MakePropRef(..)
) where

import Control.Monad.IO.Class (MonadIO)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSObjectRef, JSStringRef, JSValueRef)
import Language.Javascript.JSC.Monad (JSC)
import Language.Javascript.JSC.PropRef (JSPropRef)

-- | Anything that can be used to make a JavaScript value reference
class MakeValueRef a where
    makeValueRef :: a -> JSC JSValueRef

-- | Anything that can be used to make a JavaScript string reference
class MakeStringRef a where
    makeStringRef :: MonadIO m => a -> m JSStringRef

-- | Anything that can be used to make a list of JavaScript value
--   references for use as function arguments
class MakeArgRefs this where
    makeArgRefs :: this -> JSC [JSValueRef]

-- | Anything that can be used to make a JavaScript object reference
class MakeObjectRef this where
    makeObjectRef :: this -> JSC JSObjectRef

-- | Anything that can be used to make a JavaScript property reference
class MakePropRef this where
    makePropRef :: this -> JSC JSPropRef



