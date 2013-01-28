{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC.Object
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | Interface to JavaScript object
--
-----------------------------------------------------------------------------

module Language.Javascript.JSC.Object (
    JSObjectRef
  , MakeObjectRef(..)

  -- * Property lookup
  , (!)
  , (!!)

  -- * Setting the value of a property
  , (<#)

  -- * Calling JavaSctipt
  , (#)
  , new
  , call

  -- * Calling Haskell From JavaScript
  , function
  , fun
  , JSCallAsFunction(..)
  -- ** Object Constructors

  -- | There is no good way to support calling haskell code as a JavaScript
  --   constructor for the same reason that the return type of
  --   'JSCallAsFunction' is 'JSUndefined'.
  --
  --   Instead of writing a constructor in Haskell write a function
  --   that takes a continuation.  Create the JavaScript object
  --   and pass it to the continuation.

  -- * Arrays
  , array

  -- * Global Object
  , global

  -- * Enumerating Properties
  , copyPropertyNames
  , propertyNames

  -- * Low level
  , objCallAsFunction
  , objCallAsConstructor
) where

import Prelude hiding ((!!))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSPropertyNameArrayRef, JSStringRef, JSObjectRef, JSValueRefRef,
        JSValueRef, JSContextRef)
import Foreign.C (CSize(..), CUInt(..), CULong(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
       (jspropertynamearraygetnameatindex, jspropertynamearraygetcount,
        jsobjectcopypropertynames, jsobjectsetpropertyatindex,
        jsobjectgetpropertyatindex, jsobjectcallasconstructor,
        jsobjectmakearray, jsobjectcallasfunction, jsobjectgetproperty,
        jsobjectsetproperty, JSPropertyAttributes,
        JSObjectCallAsFunctionCallback, jsobjectmakefunctionwithcallback)
import Language.Javascript.JSC.Exception (rethrow)
import Language.Javascript.JSC.Value
       (JSUndefined, valMakeUndefined, valToObject)
import Language.Javascript.JSC.PropRef (JSPropRef(..))
import Language.Javascript.JSC.Classes
       (MakeValueRef(..), MakeStringRef(..), MakeArgRefs(..), MakePropRef(..),
        MakeObjectRef(..))
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
import Language.Javascript.JSC.Properties

-- | If we already have a JSObjectRef we are fine
instance MakeObjectRef JSObjectRef where
    makeObjectRef = return

-- | JSObjectRef can be made by evaluating a function in 'JSC' as long
--   as it returns something we can make into a JSObjectRef.
instance MakeObjectRef v => MakeObjectRef (JSC v) where
    makeObjectRef v = v >>= makeObjectRef

-- | Lookup a property based on its name.  This function just constructs a JSPropRef
--   the lookup is delayed until we use the JSPropRef.  This makes it a bit lazy compared
--   to JavaScript's @.@ operator.
--
-- >>> runjs $ eval "'Hello World'.length"
-- >>> runjs $ val "Hello World" ! "length"
-- "11"
(!) :: (MakeObjectRef this, MakeStringRef name)
    => this          -- ^ Object to look on
    -> name          -- ^ Name of the property to find
    -> JSC JSPropRef -- ^ Property reference
this ! name = do
    rthis <- makeObjectRef this
    rname <- makeStringRef name
    return (JSPropRef rthis rname)

-- | Lookup a property based on its index.  This function just constructs a JSPropRef
--   the lookup is delayed until we use the JSPropRef.  This makes it a bit lazy compared
--   to JavaScript's @[]@ operator.
--
-- >>> runjs $ eval "'Hello World'[6]"
-- >>> runjs $ val "Hello World" !! 6
-- "W"
(!!) :: (MakeObjectRef this)
     => this          -- ^ Object to look on
     -> CUInt         -- ^ Index of the property to lookup
     -> JSC JSPropRef -- ^ Property reference
this !! index = do
    rthis <- makeObjectRef this
    return (JSPropIndexRef rthis index)

-- | Call a JavaScript function
--
-- >>> runjs $ eval "'Hello World'.indexOf('World')"
-- >>> runjs $ val "Hello World" ! "indexOf" # ["World"]
-- "6"
(#) :: (MakePropRef prop, MakeArgRefs args)
    => prop -> args -> JSC JSValueRef
prop # args = do
    rprop <- makePropRef prop
    (this, f) <- objGetProperty' rprop
    rethrow $ objCallAsFunction f this args

-- | Call a JavaScript function
--
-- >>> runjs $ eval "var j = {}; j.x = 1; j.x"
-- >>> runjs $ do {j <- eval "({})"; j!"x" <# 1; j!"x"}
-- "1"
infixr 0 <#
(<#) :: (MakePropRef prop, MakeValueRef val)
     => prop          -- ^ Property to set
     -> val           -- ^ Value to set it to
     -> JSC JSPropRef -- ^ Reference to the property set
prop <# val = do
    p <- makePropRef prop
    objSetProperty p val
    return p

-- | Use this to create a new JavaScript object
--
-- >>> runjs $ new "Date" (2013, 1, 1)
-- "Fri Feb 01 2013 00:00:00 GMT+1300 (NZDT)"
new :: (MakeObjectRef constructor, MakeArgRefs args)
    => constructor
    -> args
    -> JSC JSValueRef
new constructor args = do
    f <- makeObjectRef constructor
    rethrow $ objCallAsConstructor f args

-- | Call function with a given @this@.  In most cases you should use '#'.
--
-- >>> runjs $ eval "(function(){return this;}).apply('Hello', [])"
-- >>> runjs $ do { test <- eval "(function(){return this;})"; call test (val "Hello") () }
-- "Hello"
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

-- | Type used for Haskell functions called from JavaScript.
type JSCallAsFunction = JSValueRef      -- ^ Function object
                     -> JSValueRef      -- ^ this
                     -> [JSValueRef]    -- ^ Function arguments
                     -> JSC JSUndefined -- ^ Only 'JSUndefined' can be returned because
                                        --   the function may need to be executed in a
                                        --   different thread.  If you need to get a
                                        --   value out pass in a continuation function
                                        --   as an argument and invoke it from haskell.

-- | Short hand @::JSCallAsFunction@ so a haskell function can be passed to
--   a to a JavaScipt one.
--
-- >>> runjs $ eval "(function(f) {f('Hello');})(function (a) {console.log(a)})"
-- >>> runjs $ call (eval "(function(f) {f('Hello');})") global [fun $ \ _ _ args -> valToText (head args) >>= (liftIO . print) ]
-- "Hello"
-- "undefined"
fun :: JSCallAsFunction -> JSCallAsFunction
fun = id

foreign import ccall "wrapper"
  mkJSObjectCallAsFunctionCallback :: JSObjectCallAsFunctionCallback' -> IO JSObjectCallAsFunctionCallback

-- ^ Make a JavaScript function object that wraps a Haskell function.
function :: MakeStringRef name
         => name             -- ^ Name of the function
         -> JSCallAsFunction -- ^ Haskell function to call
         -> JSC JSObjectRef  -- ^ Returns a JavaScript function object that will
                             --   call the Haskell one when it is called
function name f = do
    gctxt <- ask
    callback <- liftIO $ mkJSObjectCallAsFunctionCallback wrap
    sName <- makeStringRef name
    liftIO $ jsobjectmakefunctionwithcallback gctxt sName callback
  where
    wrap ctx fobj this argc argv exception = do
            args <- peekArray (fromIntegral argc) argv
            (`runReaderT` ctx) $
                f fobj this args >>= makeValueRef
      `E.catch` \(e :: SomeException) -> do
            str <- runReaderT (makeValueRef . T.pack $ show e) ctx
            poke exception str
            jsvaluemakeundefined ctx

-- | A callback to Haskell can be used as a JavaScript value.  This will create
--   an anonymous JavaScript function object.  Use 'function' to create one with
--   a name.
instance MakeValueRef JSCallAsFunction where
    makeValueRef = function (nullPtr::JSStringRef)

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

-- | Make an JavaScript array from a list of values
--
-- >>> runjs $ eval "['Hello', 'World'][1]"
-- >>> runjs $ array ["Hello", "World"] !! 1
-- "World"
-- >>> runjs $ eval "['Hello', null, undefined, true, 1]"
-- >>> runjs $ array ("Hello", JSNull, (), True, 1.0::Double)
-- "Hello,,,true,1"
array :: MakeArgRefs args => args -> JSC JSObjectRef
array = rethrow . makeArray

-- | JavaScript's global object
global :: JSC JSObjectRef
global = ask >>= (liftIO . jscontextgetglobalobject)

---- String global scope ----
-- | A string on its own is assumed to be the name of a property in the global object
instance MakePropRef String where
    makePropRef name = do
        this <- global
        rname <- makeStringRef name
        return (JSPropRef this rname)

-- | A string on its own is assumed to be the name of a property in the global object
instance MakeObjectRef String where
    makeObjectRef name = do
        this <- global
        rname <- makeStringRef name
        rethrow (objGetPropertyByName this rname) >>= valToObject

-- | Get an array containing the property names present on a given object
copyPropertyNames :: MakeObjectRef this => this -> JSC JSPropertyNameArrayRef
copyPropertyNames this = do
    gctxt <- ask
    rthis <- makeObjectRef this
    liftIO $ jsobjectcopypropertynames gctxt rthis

-- | Get the number of names in a property name array
propertyNamesCount :: MonadIO m => JSPropertyNameArrayRef -> m CSize
propertyNamesCount names = liftIO $ jspropertynamearraygetcount names

-- | Get a name out of a property name array
propertyNamesAt :: MonadIO m => JSPropertyNameArrayRef -> CSize -> m JSStringRef
propertyNamesAt names index = liftIO $ jspropertynamearraygetnameatindex names index

-- | Convert property array to a list
propertyNamesList :: MonadIO m => JSPropertyNameArrayRef -> m [JSStringRef]
propertyNamesList names = do
    count <- propertyNamesCount names
    mapM (propertyNamesAt names) $ enumFromTo 0 (count - 1)

-- | Get a list containing the property names present on a given object
propertyNames :: MakeObjectRef this => this -> JSC [JSStringRef]
propertyNames this = copyPropertyNames this >>= propertyNamesList

-- | Get a list containing references to all the  properties present on a given object
properties :: MakeObjectRef this => this -> JSC [JSPropRef]
properties this = propertyNames this >>= mapM (this !)

-- | Call a JavaScript object as function.  Consider using '#'.
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

-- | Call a JavaScript object as a constructor. Consider using 'new'.
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





