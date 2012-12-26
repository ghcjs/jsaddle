{-# LANGUAGE ForeignFunctionInterface
           , TypeSynonymInstances
           , FlexibleInstances
           , DeriveDataTypeable
           , ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC.Object
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSC.Object (
    objSetProperty
  , objGetProperty
  , objCallAsFunction
  , (!)
  , (!!)
  , (#)
  , (<#)
  , new
  , call
  , function
  , fun
  , array
  , global
) where

import Prelude hiding ((!!))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSStringRef, JSObjectRef, JSValueRefRef, JSValueRef, JSContextRef)
import Foreign.C (CUInt, CULong(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
       (jsobjectsetpropertyatindex, jsobjectgetpropertyatindex,
        jsobjectcallasconstructor, jsobjectmakearray,
        jsobjectcallasfunction, jsobjectgetproperty, jsobjectsetproperty,
        JSPropertyAttributes, JSObjectCallAsFunctionCallback,
        jsobjectmakefunctionwithcallback)
import Language.Javascript.JSC.Value
       (valMakeUndefined, valToObject, MakeValueRef(..),
        MakeStringRef(..), MakeArgRefs(..), rethrow)
import Language.Javascript.JSC.Monad
       (JSC)
import Control.Monad.Trans.Reader (runReaderT, ask)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign (peekArray, nullPtr, withArrayLen)
import qualified Control.Exception as E (catch)
import Control.Exception (SomeException)
import qualified Data.Text as T (pack)
import Foreign.Storable (Storable(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
       (jsvaluemakeundefined)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSContextRef
       (jscontextgetglobalobject)

---- Object Referene ----
class MakeObjectRef this where
    makeObjectRef :: this -> JSC JSObjectRef

instance MakeObjectRef JSObjectRef where
    makeObjectRef = return

instance MakeObjectRef v => MakeObjectRef (JSC v) where
    makeObjectRef v =  v >>= makeObjectRef

---- Property References ----
data JSPropRef = JSPropRef JSObjectRef JSStringRef
               | JSPropIndexRef JSObjectRef CUInt

class MakePropRef this where
    makePropRef :: this -> JSC JSPropRef

instance MakePropRef JSPropRef where
    makePropRef = return

instance MakePropRef prop => MakePropRef (JSC prop) where
    makePropRef prop = prop >>= makePropRef

instance MakeObjectRef JSPropRef where
    makeObjectRef = objGetProperty

instance MakeValueRef JSPropRef where
    makeValueRef = objGetProperty

instance MakeArgRefs JSPropRef where
    makeArgRefs p = do
        rarg <- objGetProperty p
        return [rarg]

---- String global scope ----
instance MakePropRef String where
    makePropRef name = do
        this <- global
        rname <- makeStringRef name
        return (JSPropRef this rname)

instance MakeObjectRef String where
    makeObjectRef name = do
        this <- global
        rname <- makeStringRef name
        (rethrow $ objGetPropertyByName this rname) >>= valToObject

global :: JSC JSObjectRef
global = ask >>= (liftIO . jscontextgetglobalobject)

objSetPropertyByName :: (MakeStringRef name, MakeValueRef val)
                     => JSObjectRef
                     -> name
                     -> val
                     -> JSPropertyAttributes
                     -> JSValueRefRef
                     -> JSC ()
objSetPropertyByName this name val attributes exceptions = do
    gctxt <- ask
    sName <- makeStringRef name
    vref <- makeValueRef val
    liftIO $ jsobjectsetproperty gctxt this sName vref attributes exceptions

objSetPropertyAtIndex :: (MakeValueRef val)
                      => JSObjectRef
                      -> CUInt
                      -> val
                      -> JSValueRefRef
                      -> JSC ()
objSetPropertyAtIndex this index val exceptions = do
    gctxt <- ask
    vref <- makeValueRef val
    liftIO $ jsobjectsetpropertyatindex gctxt this index vref exceptions

objSetProperty :: (MakeValueRef val)
               => JSPropRef
               -> val
               -> JSC ()
objSetProperty (JSPropRef      this name ) val = rethrow $ objSetPropertyByName  this name  val 0
objSetProperty (JSPropIndexRef this index) val = rethrow $ objSetPropertyAtIndex this index val

objGetPropertyByName :: MakeStringRef name
                     => JSObjectRef
                     -> name
                     -> JSValueRefRef
                     -> JSC JSValueRef
objGetPropertyByName this name exceptions = do
    gctxt <- ask
    sName <- makeStringRef name
    liftIO $ jsobjectgetproperty gctxt this sName exceptions

objGetPropertyAtIndex :: JSObjectRef
                      -> CUInt
                      -> JSValueRefRef
                      -> JSC JSValueRef
objGetPropertyAtIndex this index exceptions = do
    gctxt <- ask
    liftIO $ jsobjectgetpropertyatindex gctxt this index exceptions

objGetProperty :: JSPropRef
               -> JSC JSValueRef
objGetProperty (JSPropRef      this name ) = rethrow $ objGetPropertyByName  this name
objGetProperty (JSPropIndexRef this index) = rethrow $ objGetPropertyAtIndex this index

objGetProperty' :: JSPropRef
                -> JSC (JSObjectRef, JSValueRef)
objGetProperty' (JSPropRef this name) = do
    p <- rethrow $ objGetPropertyByName this name
    return (this, p)
objGetProperty' (JSPropIndexRef this index) = do
    p <- rethrow $ objGetPropertyAtIndex this index
    return (this, p)

objCallAsFunction :: MakeArgRefs args
                  => JSObjectRef
                  -> JSObjectRef
                  -> args
                  -> JSValueRefRef
                  -> JSC JSValueRef
objCallAsFunction function this args exceptions = do
    gctxt <- ask
    rargs <- makeArgRefs args
    liftIO $ withArrayLen rargs $ \ largs pargs ->
        jsobjectcallasfunction gctxt function this (fromIntegral largs) pargs exceptions

objCallAsConstructor :: MakeArgRefs args
                     => JSObjectRef
                     -> args
                     -> JSValueRefRef
                     -> JSC JSValueRef
objCallAsConstructor function args exceptions = do
    gctxt <- ask
    rargs <- makeArgRefs args
    liftIO $ withArrayLen rargs $ \ largs pargs ->
        jsobjectcallasconstructor gctxt function (fromIntegral largs) pargs exceptions

(!) :: (MakeObjectRef this, MakeStringRef name) => this -> name -> JSC JSPropRef
this ! name = do
    rthis <- makeObjectRef this
    rname <- makeStringRef name
    return (JSPropRef rthis rname)

(!!) :: (MakeObjectRef this) => this -> CUInt -> JSC JSPropRef
this !! index = do
    rthis <- makeObjectRef this
    return (JSPropIndexRef rthis index)

(#) :: (MakePropRef prop, MakeArgRefs args)
    => prop -> args -> JSC JSValueRef
prop # args = do
    rprop <- makePropRef prop
    (this, f) <- objGetProperty' rprop
    rethrow $ objCallAsFunction f this args

(<#) :: (MakePropRef prop, MakeValueRef val)
     => prop -> val -> JSC JSPropRef
prop <# val = do
    p <- makePropRef prop
    objSetProperty p val
    return p

new :: (MakeObjectRef constructor, MakeArgRefs args)
    => constructor -> args -> JSC JSValueRef
new constructor args = do
    f <- makeObjectRef constructor
    rethrow $ objCallAsConstructor f args

call :: (MakeObjectRef function, MakeObjectRef this, MakeArgRefs args)
    => function -> this -> args -> JSC JSValueRef
call function this args = do
    rfunction <- makeObjectRef function
    rthis     <- makeObjectRef this
    rethrow $ objCallAsFunction rfunction rthis args

type JSObjectCallAsFunctionCallback' =
       JSContextRef
    -> JSValueRef
    -> JSValueRef
    -> CULong
    -> JSValueRefRef
    -> JSValueRefRef
    -> IO JSValueRef

type JSCallAsFunction = JSValueRef -> JSValueRef -> [JSValueRef] -> JSC ()

foreign import ccall "wrapper"
  mkJSObjectCallAsFunctionCallback :: JSObjectCallAsFunctionCallback' -> IO JSObjectCallAsFunctionCallback

function :: MakeStringRef name
         => name
         -> JSCallAsFunction
         -> JSC JSObjectRef
function name f = do
    gctxt <- ask
    callback <- liftIO $ mkJSObjectCallAsFunctionCallback wrap
    sName <- makeStringRef name
    liftIO $ jsobjectmakefunctionwithcallback gctxt sName callback
  where
    wrap ctx fobj this argc argv exception = do
            args <- peekArray (fromIntegral argc) argv
            flip runReaderT ctx $ do
                f fobj this args >>= makeValueRef
      `E.catch` \(e :: SomeException) -> do
            str <- runReaderT (makeValueRef . T.pack $ show e) ctx
            poke exception str
            jsvaluemakeundefined ctx

fun :: JSCallAsFunction -> JSCallAsFunction
fun f = f

instance MakeValueRef JSCallAsFunction where
    makeValueRef f = function (nullPtr::JSStringRef) f

instance MakeArgRefs JSCallAsFunction where
    makeArgRefs f = do
        rarg <- function (nullPtr::JSStringRef) f
        return [rarg]

makeArray :: MakeArgRefs args => args -> JSValueRefRef -> JSC JSObjectRef
makeArray args exceptions = do
    gctxt <- ask
    rargs <- makeArgRefs args
    liftIO $ withArrayLen rargs $ \ len ptr ->
        jsobjectmakearray gctxt (fromIntegral len) ptr exceptions

array :: MakeArgRefs args => args -> JSC JSObjectRef
array = rethrow . makeArray






















