module JavaScript.Object.Internal
    ( Object(..)
    , create
--    , allProps
    , listProps
    , getProp
    , unsafeGetProp
    , setProp
    , unsafeSetProp
--    , isInstanceOf
    ) where

import Language.Javascript.JSaddle.Types (JSM, JSVal, Object(..), JSString)
import Language.Javascript.JSaddle.Native.Internal
       (newEmptyObject, propertyNames, getPropertyByName, setPropertyByName)

-- | create an empty object
create :: JSM Object
create = newEmptyObject
{-# INLINE create #-}

--allProps :: Object -> JSM JSArray
--allProps o = js_allProps o
--{-# INLINE allProps #-}

listProps :: Object -> JSM [JSString]
listProps = propertyNames
{-# INLINE listProps #-}

{- | get a property from an object. If accessing the property results in
     an exception, the exception is converted to a JSException. Since exception
     handling code prevents some optimizations in some JS engines, you may want
     to use unsafeGetProp instead
 -}
getProp :: JSString -> Object -> JSM JSVal
getProp = unsafeGetProp
{-# INLINE getProp #-}

unsafeGetProp :: JSString -> Object -> JSM JSVal
unsafeGetProp = getPropertyByName
{-# INLINE unsafeGetProp #-}

setProp :: JSString -> JSVal -> Object -> JSM ()
setProp = unsafeSetProp
{-# INLINE setProp #-}

unsafeSetProp :: JSString -> JSVal -> Object -> JSM ()
unsafeSetProp = setPropertyByName
{-# INLINE unsafeSetProp #-}

--isInstanceOf :: Object -> JSVal -> GHCJSPure Bool
--isInstanceOf o s = js_isInstanceOf o s
--{-# INLINE isInstanceOf #-}
