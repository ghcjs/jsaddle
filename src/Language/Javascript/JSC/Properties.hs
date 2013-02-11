{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC.Properties
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | Low level JavaScript object property access.  In most cases you
--   should use "Language.Javascript.JSC.Object" instead.
--
--   This module is mostly here to implement functions needed to use
--   'JSPropRef'.
--
-----------------------------------------------------------------------------

module Language.Javascript.JSC.Properties (
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

import Language.Javascript.JSC.PropRef (JSPropRef(..))
import Language.Javascript.JSC.Classes
       (MakePropRef(..), MakeObjectRef(..), MakeValueRef(..),
        MakeArgRefs(..), MakeStringRef(..))
import Language.Javascript.JSC.Monad (JSC)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSValueRefRef, JSObjectRef)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
       (jsobjectgetpropertyatindex, jsobjectgetproperty,
        jsobjectsetpropertyatindex, jsobjectsetproperty,
        JSPropertyAttributes)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C (CUInt)
import Language.Javascript.JSC.Exception (rethrow)
import Language.Javascript.JSC.Value (JSValueRef)
import Language.Javascript.JSC.Arguments ()
import Language.Javascript.JSC.String ()

-- | If we already have a JSPropRef we are fine
instance MakePropRef JSPropRef where
    makePropRef = return

-- | JSPropRef can be made by evaluating a function in 'JSC' as long
--   as it returns something we can make into a JSPropRef.
instance MakePropRef prop => MakePropRef (JSC prop) where
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
                     -> JSC JSValueRef -- ^ returns the property value.
objGetPropertyByName this name exceptions = do
    gctxt <- ask
    liftIO $ jsobjectgetproperty gctxt this (makeStringRef name) exceptions

-- | Get a property value given the object and the index of the property.
objGetPropertyAtIndex :: JSObjectRef    -- ^ object to find the property on.
                      -> CUInt          -- ^ index of the property.
                      -> JSValueRefRef  -- ^ exception if one is raised.
                      -> JSC JSValueRef -- ^ returns the property value.
objGetPropertyAtIndex this index exceptions = do
    gctxt <- ask
    liftIO $ jsobjectgetpropertyatindex gctxt this index exceptions

-- | Gets the value of a property given a 'JSPropRef'.
objGetProperty :: JSPropRef      -- ^ property reference.
               -> JSC JSValueRef -- ^ returns the property value.
objGetProperty (JSPropRef      this name ) =
    rethrow $ objGetPropertyByName  this name
objGetProperty (JSPropIndexRef this index) =
    rethrow $ objGetPropertyAtIndex this index

-- | This version of 'objGetProperty' is handy when you also need to perform.
--   another operation on the object the property is on.
objGetProperty' :: JSPropRef                     -- ^ property reference.
                -> JSC (JSObjectRef, JSValueRef) -- ^ returns the object and property value.
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
                     -> JSC ()
objSetPropertyByName this name val attributes exceptions = do
    gctxt <- ask
    vref <- makeValueRef val
    liftIO $ jsobjectsetproperty gctxt this (makeStringRef name) vref attributes exceptions

-- | Set a property value given the object and the index of the property.
objSetPropertyAtIndex :: (MakeValueRef val)
                      => JSObjectRef    -- ^ object to find property on.
                      -> CUInt          -- ^ index of the property.
                      -> val            -- ^ new value to set the property to.
                      -> JSValueRefRef  -- ^ exception if one is raised.
                      -> JSC ()
objSetPropertyAtIndex this index val exceptions = do
    gctxt <- ask
    vref <- makeValueRef val
    liftIO $ jsobjectsetpropertyatindex gctxt this index vref exceptions

-- | Sets the value of a property given a 'JSPropRef'.
objSetProperty :: (MakeValueRef val)
               => JSPropRef -- ^ property reference.
               -> val       -- ^ new value to set the property to.
               -> JSC ()
objSetProperty (JSPropRef      this name ) val = rethrow $ objSetPropertyByName  this name  val 0
objSetProperty (JSPropIndexRef this index) val = rethrow $ objSetPropertyAtIndex this index val





