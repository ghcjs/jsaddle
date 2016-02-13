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
import Language.Javascript.JSaddle.Types
       (MutableJSArray, Object(..), JSPropertyAttributes,
        Index)
#ifdef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Types (JSString)
#else
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
       (jsobjectgetpropertyatindex, jsobjectgetproperty,
        jsobjectsetpropertyatindex, jsobjectsetproperty)
import Language.Javascript.JSaddle.Native
       (makeNewJSVal, withObject, withJSString, withToJSVal)
#endif
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Language.Javascript.JSaddle.Value (JSVal)
import Language.Javascript.JSaddle.Arguments ()
import Language.Javascript.JSaddle.String ()

-- | Get a property value given the object and the name of the property.
objGetPropertyByName :: ToJSString name
                     => Object         -- ^ object to find the property on.
                     -> name           -- ^ name of the property.
                     -> MutableJSArray -- ^ exception if one is raised.
                     -> JSM JSVal      -- ^ returns the property value.
#ifdef ghcjs_HOST_OS
objGetPropertyByName this name = liftIO . js_tryGetProp (toJSString name) this
{-# INLINE objGetPropertyByName #-}
foreign import javascript unsafe "try { $r=$2[$1] } catch(e) { $3[0] = e }"
    js_tryGetProp :: JSString -> Object -> MutableJSArray -> IO JSVal
#else
objGetPropertyByName this name exceptions = do
    gctxt <- ask
    withObject this $ \rthis ->
        withJSString (toJSString name) $ \name' ->
            (liftIO $ jsobjectgetproperty gctxt rthis name' exceptions) >>= makeNewJSVal
{-# INLINE objGetPropertyByName #-}
#endif

-- | Get a property value given the object and the index of the property.
objGetPropertyAtIndex :: Object         -- ^ object to find the property on.
                      -> Index          -- ^ index of the property.
                      -> MutableJSArray -- ^ exception if one is raised.
                      -> JSM JSVal      -- ^ returns the property value.
#ifdef ghcjs_HOST_OS
objGetPropertyAtIndex this index = liftIO . js_tryIndex index this
{-# INLINE objGetPropertyAtIndex #-}
foreign import javascript unsafe "try { $r=$2[$1] } catch(e) { $3[0] = e }"
    js_tryIndex :: Index -> Object -> MutableJSArray -> IO JSVal
#else
objGetPropertyAtIndex this index exceptions = do
    gctxt <- ask
    withObject this $ \rthis ->
        (liftIO $ jsobjectgetpropertyatindex gctxt rthis index exceptions) >>= makeNewJSVal
{-# INLINE objGetPropertyAtIndex #-}
#endif

-- | Set a property value given the object and the name of the property.
objSetPropertyByName :: (ToJSString name, ToJSVal val)
                     => Object               -- ^ object to set the property on.
                     -> name                 -- ^ name of the property.
                     -> val                  -- ^ new value to set the property to.
                     -> JSPropertyAttributes -- ^ property attributes to give the property.
                     -> MutableJSArray       -- ^ exception if one is raised.
                     -> JSM ()
#ifdef ghcjs_HOST_OS
objSetPropertyByName this name val attributes exceptions = do
    vref <- toJSVal val
    liftIO $ js_trySetProp (toJSString name) this vref exceptions
{-# INLINE objSetPropertyByName #-}
foreign import javascript unsafe "try { $2[$1]=$3 } catch(e) { $4[0] = e }"
    js_trySetProp :: JSString -> Object -> JSVal -> MutableJSArray -> IO ()
#else
objSetPropertyByName this name val attributes exceptions = do
    gctxt <- ask
    withObject this $ \rthis ->
        withJSString (toJSString name) $ \name' ->
        withToJSVal val $ \rval ->
            liftIO $ jsobjectsetproperty gctxt rthis name' rval attributes exceptions
{-# INLINE objSetPropertyByName #-}
#endif

-- | Set a property value given the object and the index of the property.
objSetPropertyAtIndex :: (ToJSVal val)
                      => Object         -- ^ object to find property on.
                      -> Index          -- ^ index of the property.
                      -> val            -- ^ new value to set the property to.
                      -> MutableJSArray -- ^ exception if one is raised.
                      -> JSM ()
#ifdef ghcjs_HOST_OS
objSetPropertyAtIndex this index val exceptions = do
    vref <- toJSVal val
    liftIO $ js_trySetAtIndex index this vref exceptions
{-# INLINE objSetPropertyAtIndex #-}
foreign import javascript unsafe "try { $2[$1]=$3 } catch(e) { $4[0] = e }"
    js_trySetAtIndex :: Index -> Object -> JSVal -> MutableJSArray -> IO ()
#else
objSetPropertyAtIndex this index val exceptions = do
    gctxt <- ask
    withObject this $ \rthis ->
        withToJSVal val $ \rval ->
            liftIO $ jsobjectsetpropertyatindex gctxt rthis index rval exceptions
{-# INLINE objSetPropertyAtIndex #-}
#endif

