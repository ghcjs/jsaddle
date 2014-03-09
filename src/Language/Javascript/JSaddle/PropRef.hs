-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.PropRef
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | Implements a reference to a property
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.PropRef (
    JSPropRef(..)
) where

import Language.Javascript.JSaddle.Types
       (JSStringRef, JSObjectRef, Index)

-- | A reference to a property.
--   Implemented as a reference to an object and something to find the property.
data JSPropRef = JSPropRef JSObjectRef JSStringRef -- ^ Object and property name.
               | JSPropIndexRef JSObjectRef Index  -- ^ Object and property index.


