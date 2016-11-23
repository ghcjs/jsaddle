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
    getProp, unsafeGetProp
  , objGetPropertyByName
  , objGetPropertyAtIndex
  -- * Setting Property Values
  , setProp, unsafeSetProp
  , objSetPropertyByName
  , objSetPropertyAtIndex
) where

import Language.Javascript.JSaddle.Monad (JSM)
import Language.Javascript.JSaddle.Types (JSVal, Object(..))
import JavaScript.Object.Internal (getProp, unsafeGetProp, setProp, unsafeSetProp)
#ifdef ghcjs_HOST_OS
import GHCJS.Marshal (ToJSVal(..))
#else
import GHCJS.Marshal.Internal (ToJSVal(..))
import Language.Javascript.JSaddle.Native
       (withObject, withToJSVal)
import Language.Javascript.JSaddle.Run
       (AsyncCommand(..), sendLazyCommand, sendAsyncCommand)
#endif
import Language.Javascript.JSaddle.Arguments ()
import Language.Javascript.JSaddle.String ()
import Language.Javascript.JSaddle.Marshal.String (ToJSString(..))

-- | Get a property value given the object and the name of the property.
objGetPropertyByName :: ToJSString name
                     => Object         -- ^ object to find the property on.
                     -> name           -- ^ name of the property.
                     -> JSM JSVal      -- ^ returns the property value.
objGetPropertyByName this name = unsafeGetProp (toJSString name) this

-- | Get a property value given the object and the index of the property.
objGetPropertyAtIndex :: Object    -- ^ object to find the property on.
                      -> Int       -- ^ index of the property.
                      -> JSM JSVal -- ^ returns the property value.
#ifdef ghcjs_HOST_OS
objGetPropertyAtIndex this index = js_tryIndex index this
foreign import javascript unsafe "$r=$2[$1]"
    js_tryIndex :: Int -> Object -> IO JSVal
#else
objGetPropertyAtIndex this index =
    withObject this $ \rthis -> sendLazyCommand $ GetPropertyAtIndex rthis index
#endif

-- | Set a property value given the object and the name of the property.
objSetPropertyByName :: (ToJSString name, ToJSVal val)
                     => Object               -- ^ object to set the property on.
                     -> name                 -- ^ name of the property.
                     -> val                  -- ^ new value to set the property to.
                     -> JSM ()
objSetPropertyByName this name val = do
    vref <- toJSVal val
    unsafeSetProp (toJSString name) vref this

-- | Set a property value given the object and the index of the property.
objSetPropertyAtIndex :: (ToJSVal val)
                      => Object         -- ^ object to find property on.
                      -> Int          -- ^ index of the property.
                      -> val            -- ^ new value to set the property to.
                      -> JSM ()
#ifdef ghcjs_HOST_OS
objSetPropertyAtIndex this index val = do
    vref <- toJSVal val
    js_trySetAtIndex index this vref
foreign import javascript unsafe "$2[$1]=$3"
    js_trySetAtIndex :: Int -> Object -> JSVal -> IO ()
#else
objSetPropertyAtIndex this index val =
    withObject this $ \rthis ->
        withToJSVal val $ \rval ->
            sendAsyncCommand $ SetPropertyAtIndex rthis index rval
#endif

