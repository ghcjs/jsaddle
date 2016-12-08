module JavaScript.Object.Internal
    ( Object(..)
    , create
--    , allProps
    , listProps
    , getProp
    , unsafeGetProp
    , setProp
    , unsafeSetProp
--    , isInstanceOfIO
    ) where

import Language.Javascript.JSaddle.Types (JSM, JSVal, Object(..), JSString)
import Language.Javascript.JSaddle.Native.Internal
       (withObject, withJSString, withJSVal, wrapJSString)
import Language.Javascript.JSaddle.Run
       (Command(..), AsyncCommand(..), Result(..), sendCommand, sendLazyCommand, sendAsyncCommand)

create :: JSM Object
create = Object <$> sendLazyCommand NewEmptyObject

listProps :: Object -> JSM [JSString]
listProps this =
    withObject this $ \rthis -> do
        ~(PropertyNamesResult result) <- sendCommand $ PropertyNames rthis
        mapM wrapJSString result
{-# INLINE listProps #-}

unsafeGetProp :: JSString -> Object -> JSM JSVal
unsafeGetProp name this =
    withObject this $ \rthis ->
        withJSString name $ sendLazyCommand . GetPropertyByName rthis
{-# INLINE unsafeGetProp #-}

getProp :: JSString -> Object -> JSM JSVal
getProp = unsafeGetProp
{-# INLINE getProp #-}

unsafeSetProp :: JSString -> JSVal -> Object -> JSM ()
unsafeSetProp name val this =
    withObject this $ \rthis ->
        withJSString name $ \rname ->
            withJSVal val $ \rval ->
                sendAsyncCommand $ SetPropertyByName rthis rname rval
{-# INLINE unsafeSetProp #-}

setProp :: JSString -> JSVal -> Object -> JSM ()
setProp = unsafeSetProp
{-# INLINE setProp #-}
