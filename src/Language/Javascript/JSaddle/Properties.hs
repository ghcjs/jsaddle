{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
#endif
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Properties
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | Low level JavaScript object property access.  In most cases you
--   should use "Language.Javascript.JSaddle.Object" instead.
--
--   This module is mostly here to implement functions needed to use
--   'JSPropRef'.
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Properties (
  -- * Getting Property Values
    objGetPropertyByName
  , objGetPropertyAtIndex
  -- * Setting Property Values
  , objSetPropertyByName
  , objSetPropertyAtIndex
) where

import Language.Javascript.JSaddle.Classes
       (ToJSVal(..), ToJSString(..))
import Language.Javascript.JSaddle.Monad (JSM)
import Language.Javascript.JSaddle.Types (Object(..), Index)
#ifdef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Types (JSString)
#else
import Language.Javascript.JSaddle.Native
       (wrapJSVal, withObject, withJSString, withToJSVal)
import Language.Javascript.JSaddle.WebSockets
       (Command(..), AsyncCommand(..), Result(..), sendCommand, sendAsyncCommand)
#endif
import Control.Monad.Trans.Reader (ask)
import Language.Javascript.JSaddle.Value (JSVal)
import Language.Javascript.JSaddle.Arguments ()
import Language.Javascript.JSaddle.String ()

-- | Get a property value given the object and the name of the property.
objGetPropertyByName :: ToJSString name
                     => Object         -- ^ object to find the property on.
                     -> name           -- ^ name of the property.
                     -> JSM JSVal      -- ^ returns the property value.
#ifdef ghcjs_HOST_OS
objGetPropertyByName this name = liftIO . js_tryGetProp (toJSString name) this
foreign import javascript unsafe "try { $r=$2[$1] } catch(e) { $3[0] = e }"
    js_tryGetProp :: JSString -> Object -> MutableJSArray -> IO JSVal
#else
objGetPropertyByName this name = do
    let name' = toJSString name
    withObject this $ \rthis ->
        withJSString name' $ \rname -> do
            GetPropertyByNameResult result <- sendCommand $ GetPropertyByName rthis rname
            wrapJSVal result
#endif

-- | Get a property value given the object and the index of the property.
objGetPropertyAtIndex :: Object         -- ^ object to find the property on.
                      -> Index          -- ^ index of the property.
                      -> JSM JSVal      -- ^ returns the property value.
#ifdef ghcjs_HOST_OS
objGetPropertyAtIndex this index = liftIO . js_tryIndex index this
foreign import javascript unsafe "try { $r=$2[$1] } catch(e) { $3[0] = e }"
    js_tryIndex :: Index -> Object -> MutableJSArray -> IO JSVal
#else
objGetPropertyAtIndex this index =
    withObject this $ \rthis -> do
        GetPropertyAtIndexResult result <- sendCommand $ GetPropertyAtIndex rthis index
        wrapJSVal result
#endif

-- | Set a property value given the object and the name of the property.
objSetPropertyByName :: (ToJSString name, ToJSVal val)
                     => Object               -- ^ object to set the property on.
                     -> name                 -- ^ name of the property.
                     -> val                  -- ^ new value to set the property to.
                     -> JSM ()
#ifdef ghcjs_HOST_OS
objSetPropertyByName this name val = do
    vref <- toJSVal val
    liftIO $ js_trySetProp (toJSString name) this vref
foreign import javascript unsafe "try { $2[$1]=$3 } catch(e) { $4[0] = e }"
    js_trySetProp :: JSString -> Object -> JSVal -> MutableJSArray -> IO ()
#else
objSetPropertyByName this name val = do
    let name' = toJSString name
    withObject this $ \rthis ->
        withJSString name' $ \rname ->
            withToJSVal val $ \rval ->
                sendAsyncCommand $ SetPropertyByName rthis rname rval
#endif

-- | Set a property value given the object and the index of the property.
objSetPropertyAtIndex :: (ToJSVal val)
                      => Object         -- ^ object to find property on.
                      -> Index          -- ^ index of the property.
                      -> val            -- ^ new value to set the property to.
                      -> JSM ()
#ifdef ghcjs_HOST_OS
objSetPropertyAtIndex this index val = do
    vref <- toJSVal val
    liftIO $ js_trySetAtIndex index this vref
foreign import javascript unsafe "try { $2[$1]=$3 } catch(e) { $4[0] = e }"
    js_trySetAtIndex :: Index -> Object -> JSVal -> MutableJSArray -> IO ()
#else
objSetPropertyAtIndex this index val = do
    gctxt <- ask
    withObject this $ \rthis ->
        withToJSVal val $ \rval ->
            sendAsyncCommand $ SetPropertyAtIndex rthis index rval
#endif

