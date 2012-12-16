{-# LANGUAGE ForeignFunctionInterface
           , TypeSynonymInstances
           , FlexibleInstances
           , DeriveDataTypeable
           , ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Object
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Object (
    objSetProperty
  , objGetProperty
  , objCallAsFunction
  , (!)
  , (#)
  , (<#)
  , new
  , function
  , fun
  , array
  , global
) where

import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSStringRef, JSObjectRef, JSValueRefRef, JSValueRef, JSContextRef)
import Foreign.C (CULong(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
       (jsobjectcallasconstructor, jsobjectmakearray,
        jsobjectcallasfunction, jsobjectgetproperty, jsobjectsetproperty,
        JSPropertyAttributes, JSObjectCallAsFunctionCallback,
        jsobjectmakefunctionwithcallback)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Value
       (valMakeUndefined, valToObject, MakeValueRef(..),
        MakeStringRef(..), MakeArgRefs(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSC.Monad
       (rethrow, JSC)
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

class MakePropRef this where
    makePropRef :: this -> JSC JSPropRef

instance MakePropRef JSPropRef where
    makePropRef (JSPropRef this name) = do
        rthis <- makeObjectRef this
        rname <- makeStringRef name
        return (JSPropRef rthis rname)

instance MakePropRef prop => MakePropRef (JSC prop) where
    makePropRef prop =  prop >>= makePropRef

instance MakeObjectRef JSPropRef where
    makeObjectRef (JSPropRef this name) = rethrow $ objGetProperty this name

instance MakeValueRef JSPropRef where
    makeValueRef (JSPropRef this name) = rethrow $ objGetProperty this name

instance MakeArgRefs JSPropRef where
    makeArgRefs (JSPropRef this name) = do
        rarg <- rethrow $ objGetProperty this name
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
        (rethrow $ objGetProperty this rname) >>= valToObject

global :: JSC JSObjectRef
global = ask >>= (liftIO . jscontextgetglobalobject)

objSetProperty :: (MakeStringRef name, MakeValueRef val)
               => JSObjectRef
               -> name
               -> val
               -> JSPropertyAttributes
               -> JSValueRefRef
               -> JSC ()
objSetProperty this name val attributes exceptions = do
    gctxt <- ask
    sName <- makeStringRef name
    vref <- makeValueRef val
    liftIO $ jsobjectsetproperty gctxt this sName vref attributes exceptions

objGetProperty :: MakeStringRef name
               => JSObjectRef
               -> name
               -> JSValueRefRef
               -> JSC JSValueRef
objGetProperty this name exceptions = do
    gctxt <- ask
    sName <- makeStringRef name
    liftIO $ jsobjectgetproperty gctxt this sName exceptions

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

(#) :: (MakePropRef prop, MakeArgRefs args)
    => prop -> args -> JSC JSValueRef
prop # args = do
    (JSPropRef this name) <- makePropRef prop
    f <- (rethrow $ objGetProperty this name) >>= valToObject
    rethrow $ objCallAsFunction f this args

(<#) :: (MakePropRef prop, MakeValueRef val)
     => prop -> val -> JSC ()
prop <# val = do
    (JSPropRef this name) <- makePropRef prop
    rethrow $ objSetProperty this name val 0

new :: (MakeObjectRef constructor, MakeArgRefs args)
    => constructor -> args -> JSC JSValueRef
new constructor args = do
    f <- makeObjectRef constructor
    rethrow $ objCallAsConstructor f args

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

array :: MakeArgRefs args => args -> JSC JSValueRef
array = rethrow . makeArray























