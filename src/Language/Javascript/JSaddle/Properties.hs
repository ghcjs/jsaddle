{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
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
  -- * Propery Reference
    JSPropRef(..)
  , MakePropRef(..)

  -- * Getting Property Values
  , objGetPropertyByName
  , objGetPropertyAtIndex
  , objGetProperty
  , objGetProperty'
  -- * Setting Property Values
  , objSetPropertyByName
  , objSetPropertyAtIndex
  , objSetProperty
) where

import Control.Applicative ((<$>))
import Language.Javascript.JSaddle.PropRef (JSPropRef(..))
import Language.Javascript.JSaddle.Classes
       (MakePropRef(..), MakeObjectRef(..), MakeValueRef(..),
        MakeArgRefs(..), MakeStringRef(..))
import Language.Javascript.JSaddle.Monad (JSM)
import Language.Javascript.JSaddle.Types
       (JSValueRefRef, JSObjectRef, JSPropertyAttributes,
        Index(..), JSStringRef)
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
#else
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
       (jsobjectgetpropertyatindex, jsobjectgetproperty,
        jsobjectsetpropertyatindex, jsobjectsetproperty,
        JSPropertyAttributes)
#endif
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Language.Javascript.JSaddle.Exception (rethrow)
import Language.Javascript.JSaddle.Value (JSValueRef)
import Language.Javascript.JSaddle.Arguments ()
import Language.Javascript.JSaddle.String ()

-- | If we already have a JSPropRef we are fine
instance MakePropRef JSPropRef where
    makePropRef = return

-- | JSPropRef can be made by evaluating a function in 'JSM' as long
--   as it returns something we can make into a JSPropRef.
instance MakePropRef prop => MakePropRef (JSM prop) where
    makePropRef prop = prop >>= makePropRef

-- | We can use a property as an object.
instance MakeObjectRef JSPropRef where
    makeObjectRef = objGetProperty

-- | We can use a property as a value.
instance MakeValueRef JSPropRef where
    makeValueRef = objGetProperty

-- | We can pass a property as the only paramter to a function.
instance MakeArgRefs JSPropRef where
    makeArgRefs p = do
        rarg <- objGetProperty p
        return [rarg]

-- | Get a property value given the object and the name of the property.
objGetPropertyByName :: MakeStringRef name
                     => JSObjectRef    -- ^ object to find the property on.
                     -> name           -- ^ name of the property.
                     -> JSValueRefRef  -- ^ exception if one is raised.
                     -> JSM JSValueRef -- ^ returns the property value.
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
objGetPropertyByName this name = liftIO . js_tryGetProp (makeStringRef name) this
foreign import javascript unsafe "try { $r=$2[$1] } catch(e) { $3[0] = e }"
    js_tryGetProp :: JSStringRef -> JSObjectRef -> JSValueRefRef -> IO JSValueRef
#elif defined(USE_WEBKIT)
objGetPropertyByName this name exceptions = do
    gctxt <- ask
    liftIO $ jsobjectgetproperty gctxt this (makeStringRef name) exceptions
#else
objGetPropertyByName = undefined
#endif

-- | Get a property value given the object and the index of the property.
objGetPropertyAtIndex :: JSObjectRef    -- ^ object to find the property on.
                      -> Index          -- ^ index of the property.
                      -> JSValueRefRef  -- ^ exception if one is raised.
                      -> JSM JSValueRef -- ^ returns the property value.
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
objGetPropertyAtIndex this index = liftIO . js_tryIndex index this
foreign import javascript unsafe "try { $r=$2[$1] } catch(e) { $3[0] = e }"
    js_tryIndex :: Index -> JSObjectRef -> JSValueRefRef -> IO JSValueRef
#elif defined(USE_WEBKIT)
objGetPropertyAtIndex this index exceptions = do
    gctxt <- ask
    liftIO $ jsobjectgetpropertyatindex gctxt this index exceptions
#else
objGetPropertyAtIndex = undefined
#endif

-- | Gets the value of a property given a 'JSPropRef'.
objGetProperty :: JSPropRef      -- ^ property reference.
               -> JSM JSValueRef -- ^ returns the property value.
objGetProperty (JSPropRef      this name ) =
    rethrow $ objGetPropertyByName  this name
objGetProperty (JSPropIndexRef this index) =
    rethrow $ objGetPropertyAtIndex this index

-- | This version of 'objGetProperty' is handy when you also need to perform.
--   another operation on the object the property is on.
objGetProperty' :: JSPropRef                     -- ^ property reference.
                -> JSM (JSObjectRef, JSValueRef) -- ^ returns the object and property value.
objGetProperty' (JSPropRef this name) = do
    p <- rethrow $ objGetPropertyByName this name
    return (this, p)
objGetProperty' (JSPropIndexRef this index) = do
    p <- rethrow $ objGetPropertyAtIndex this index
    return (this, p)


-- | Set a property value given the object and the name of the property.
objSetPropertyByName :: (MakeStringRef name, MakeValueRef val)
                     => JSObjectRef          -- ^ object to set the property on.
                     -> name                 -- ^ name of the property.
                     -> val                  -- ^ new value to set the property to.
                     -> JSPropertyAttributes -- ^ property attributes to give the property.
                     -> JSValueRefRef        -- ^ exception if one is raised.
                     -> JSM ()
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
objSetPropertyByName this name val attributes exceptions = do
    vref <- makeValueRef val
    liftIO $ js_trySetProp (makeStringRef name) this vref exceptions
foreign import javascript unsafe "try { $2[$1]=$3 } catch(e) { $4[0] = e }"
    js_trySetProp :: JSStringRef -> JSObjectRef -> JSValueRef -> JSValueRefRef -> IO ()
#elif defined(USE_WEBKIT)
objSetPropertyByName this name val attributes exceptions = do
    gctxt <- ask
    vref <- makeValueRef val
    liftIO $ jsobjectsetproperty gctxt this (makeStringRef name) vref attributes exceptions
#else
objSetPropertyByName = undefined
#endif

-- | Set a property value given the object and the index of the property.
objSetPropertyAtIndex :: (MakeValueRef val)
                      => JSObjectRef    -- ^ object to find property on.
                      -> Index          -- ^ index of the property.
                      -> val            -- ^ new value to set the property to.
                      -> JSValueRefRef  -- ^ exception if one is raised.
                      -> JSM ()
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
objSetPropertyAtIndex this index val exceptions = do
    vref <- makeValueRef val
    liftIO $ js_trySetAtIndex index this vref exceptions
foreign import javascript unsafe "try { $2[$1]=$3 } catch(e) { $4[0] = e }"
    js_trySetAtIndex :: Index -> JSObjectRef -> JSValueRef -> JSValueRefRef -> IO ()
#elif defined(USE_WEBKIT)
objSetPropertyAtIndex this index val exceptions = do
    gctxt <- ask
    vref <- makeValueRef val
    liftIO $ jsobjectsetpropertyatindex gctxt this index vref exceptions
#else
objSetPropertyAtIndex = undefined
#endif

-- | Sets the value of a property given a 'JSPropRef'.
objSetProperty :: (MakeValueRef val)
               => JSPropRef -- ^ property reference.
               -> val       -- ^ new value to set the property to.
               -> JSM ()
objSetProperty (JSPropRef      this name ) val = rethrow $ objSetPropertyByName  this name  val 0
objSetProperty (JSPropIndexRef this index) val = rethrow $ objSetPropertyAtIndex this index val





