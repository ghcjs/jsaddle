-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC.PropRef
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | Implements a reference to a property
--
-----------------------------------------------------------------------------

module Language.Javascript.JSC.PropRef (
    JSPropRef(..)
) where
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSStringRef, JSObjectRef)
import Foreign.C (CUInt)

-- | A reference to a property.
--   Implemented as a reference to an object and something to find the property.
data JSPropRef = JSPropRef JSObjectRef JSStringRef -- ^ Object and property name.
               | JSPropIndexRef JSObjectRef CUInt  -- ^ Object and property index.


