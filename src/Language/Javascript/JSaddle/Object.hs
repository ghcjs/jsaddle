{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Object
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | Interface to JavaScript object
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Object (
    Object
  , MakeObject(..)

  -- * Property lookup
  , (!)
  , (!!)
  , js
  , jss
  , JSF
  , jsf
  , js0
  , js1
  , js2
  , js3
  , js4
  , js5
  , jsg
  , jsgf
  , jsg0
  , jsg1
  , jsg2
  , jsg3
  , jsg4
  , jsg5

  -- * Setting the value of a property
  , (<#)
  , (<##)

  -- * Calling JavaSctipt
  , (#)
  , (##)
  , new
  , call
  , obj

  -- * Calling Haskell From JavaScript
  , Function(..)
  , function
  , freeFunction
  , fun
  , JSCallAsFunction
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
  , propertyNames
  , properties

  -- * Low level
  , objCallAsFunction
  , objCallAsConstructor
  , nullObject
) where

import Prelude hiding ((!!))
import Language.Javascript.JSaddle.Types
       (JSPropertyNameArray, JSString, Object(..), MutableJSArray,
        JSVal, Index)
import Foreign.C.Types (CSize(..), CULong(..))
#ifdef ghcjs_HOST_OS
import GHCJS.Types (nullRef, jsval)
import GHCJS.Foreign.Callback
       (releaseCallback, syncCallback2, OnBlocked(..), Callback)
import GHCJS.Marshal.Pure (pFromJSVal)
import JavaScript.Array (JSArray)
import qualified JavaScript.Array as Array (toListIO, fromListIO)
import JavaScript.Array.Internal (SomeJSArray(..))
import qualified JavaScript.Object as Object (create)
import Control.Monad (liftM)
import Control.Applicative ((<$>))
import Data.Coerce (coerce)
#else
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
       (jsobjectmake, jspropertynamearraygetnameatindex,
        jspropertynamearraygetcount, jsobjectcopypropertynames,
        jsobjectcallasconstructor, jsobjectmakearray,
        jsobjectcallasfunction,
        JSObjectCallAsFunctionCallback,
        jsobjectmakefunctionwithcallback, JSObjectCallAsFunctionCallback')
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSValueRef
       (jsvaluemakeundefined)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSContextRef
       (jscontextgetglobalobject)
import Foreign (peekArray, nullPtr, withArrayLen)
import Foreign.Ptr (freeHaskellFunPtr)
import Language.Javascript.JSaddle.Native
       (makeNewJSVal, wrapJSString, withJSVals, withObject, withJSString,
        withToJSVal)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.ForeignPtr (newForeignPtr_)
#endif
import Language.Javascript.JSaddle.Exception (rethrow)
import Language.Javascript.JSaddle.Value
       (JSUndefined, valToObject)
import Language.Javascript.JSaddle.Classes
       (ToJSVal(..), ToJSString(..), MakeObject(..))
import Language.Javascript.JSaddle.Arguments (MakeArgs(..))
import Language.Javascript.JSaddle.Monad
       (JSM)
import Control.Monad.Trans.Reader (runReaderT, ask)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Exception as E (catch)
import Control.Exception (SomeException)
import Foreign.Storable (Storable(..))
import Language.Javascript.JSaddle.Properties
import Control.Lens (IndexPreservingGetter, to)
import Language.Javascript.JSaddle.String (nullJSString)
import Data.Text (Text)

-- $setup
-- >>> import Language.Javascript.JSaddle.Test (testJSaddle)
-- >>> import Language.Javascript.JSaddle.Evaluate (eval)
-- >>> import Language.Javascript.JSaddle.Value (val)
-- >>> import Control.Lens.Operators ((^.))

-- | Object can be made by evaluating a fnction in 'JSM' as long
--   as it returns something we can make into a Object.
instance MakeObject v => MakeObject (JSM v) where
    makeObject v = v >>= makeObject
    {-# INLINE makeObject #-}

-- | Lookup a property based on its name.
--
-- >>> testJSaddle $ eval "'Hello World'.length"
-- >>> testJSaddle $ val "Hello World" ! "length"
-- 11
(!) :: (MakeObject this, ToJSString name)
    => this           -- ^ Object to look on
    -> name           -- ^ Name of the property to find
    -> JSM JSVal -- ^ Property reference
this ! name = do
    rthis <- makeObject this
    rethrow $ objGetPropertyByName rthis rname
  where
    rname = toJSString name
{-# INLINE (!) #-}

-- | Lookup a property based on its index.
--
-- >>> testJSaddle $ eval "'Hello World'[6]"
-- >>> testJSaddle $ val "Hello World" !! 6
-- W
(!!) :: (MakeObject this)
     => this           -- ^ Object to look on
     -> Index          -- ^ Index of the property to lookup
     -> JSM JSVal -- ^ Property reference
this !! index = do
    rthis <- makeObject this
    rethrow $ objGetPropertyAtIndex rthis index
{-# INLINE (!!) #-}

-- | Makes a getter for a particular property name.
--
-- > js name = to (!name)
--
-- >>> testJSaddle $ eval "'Hello World'.length"
-- >>> testJSaddle $ val "Hello World" ^. js "length"
-- 11
js :: (MakeObject s, ToJSString name)
   => name          -- ^ Name of the property to find
   -> IndexPreservingGetter s (JSM JSVal)
js name = to (!name)
{-# INLINE js #-}

-- | Makes a setter for a particular property name.
--
-- > jss name = to (<#name)
--
-- >>> testJSaddle $ eval "'Hello World'.length"
-- >>> testJSaddle $ val "Hello World" ^. js "length"
-- 11
jss :: (ToJSString name, ToJSVal val)
   => name          -- ^ Name of the property to find
   -> val
   -> forall o . MakeObject o => IndexPreservingGetter o (JSM ())
jss name val = to (\o -> o <# name $ val)
{-# INLINE jss #-}

-- | Handy way to call a function
--
-- > jsf name = to (\o -> o # name $ args)
--
-- >>> testJSaddle $ val "Hello World" ^. jsf "indexOf" ["World"]
-- 6
jsf :: (ToJSString name, MakeArgs args) => name -> args -> JSF
jsf name args = to (\o -> o # name $ args)
{-# INLINE jsf #-}

-- | Java script function applications have this type
type JSF = forall o . MakeObject o => IndexPreservingGetter o (JSM JSVal)

-- | Handy way to call a function that expects no arguments
--
-- > js0 name = jsf name ()
--
-- >>> testJSaddle $ val "Hello World" ^. js0 "toLowerCase"
-- hello world
js0 :: (ToJSString name) => name -> JSF
js0 name = jsf name ()
{-# INLINE js0 #-}

-- | Handy way to call a function that expects one argument
--
-- > js1 name a0 = jsf name [a0]
--
-- >>> testJSaddle $ val "Hello World" ^. js1 "indexOf" "World"
-- 6
js1 :: (ToJSString name, ToJSVal a0) => name -> a0 -> JSF
js1 name a0 = jsf name [a0]
{-# INLINE js1 #-}

-- | Handy way to call a function that expects two arguments
js2 :: (ToJSString name, ToJSVal a0, ToJSVal a1) => name -> a0 -> a1 -> JSF
js2 name a0 a1 = jsf name (a0, a1)
{-# INLINE js2 #-}

-- | Handy way to call a function that expects three arguments
js3 :: (ToJSString name, ToJSVal a0, ToJSVal a1, ToJSVal a2)
    => name -> a0 -> a1 -> a2 -> JSF
js3 name a0 a1 a2 = jsf name (a0, a1, a2)
{-# INLINE js3 #-}

-- | Handy way to call a function that expects four arguments
js4 :: (ToJSString name, ToJSVal a0, ToJSVal a1, ToJSVal a2,
        ToJSVal a3)
    => name -> a0 -> a1 -> a2 -> a3 -> JSF
js4 name a0 a1 a2 a3 = jsf name (a0, a1, a2, a3)
{-# INLINE js4 #-}

-- | Handy way to call a function that expects five arguments
js5 :: (ToJSString name, ToJSVal a0, ToJSVal a1, ToJSVal a2,
        ToJSVal a3, ToJSVal a4)
    => name -> a0 -> a1 -> a2 -> a3 -> a4 -> JSF
js5 name a0 a1 a2 a3 a4 = jsf name (a0, a1, a2, a3, a4)
{-# INLINE js5 #-}


-- | Handy way to get and hold onto a reference top level javascript
--
-- >>> testJSaddle $ eval "w = console; w.log('Hello World')"
-- >>> testJSaddle $ do w <- jsg "console"; w ^. js "log" # ["Hello World"]
-- 11
jsg :: ToJSString a => a -> JSM JSVal
jsg name = global ! name
{-# INLINE jsg #-}

-- | Handy way to call a function
--
-- > jsgf name = jsg name . to (# args)
--
-- >>> testJSaddle $ eval "globalFunc = function(x) {x.length}"
-- >>> testJSaddle $ jsgf "globalFunc" ["World"]
-- 6
jsgf :: (ToJSString name, MakeArgs args) => name -> args -> JSM JSVal
jsgf name = global # name
{-# INLINE jsgf #-}

-- | Handy way to call a function that expects no arguments
--
-- > jsg0 name = jsgf name ()
--
-- >>> testJSaddle $ jsg0 "globalFunc"
-- hello world
jsg0 :: (ToJSString name) => name -> JSM JSVal
jsg0 name = jsgf name ()
{-# INLINE jsg0 #-}

-- | Handy way to call a function that expects one argument
--
-- > jsg1 name a0 = jsgf name [a0]
--
-- >>> testJSaddle $ jsg1 "globalFunc" "World"
-- 6
jsg1 :: (ToJSString name, ToJSVal a0) => name -> a0 -> JSM JSVal
jsg1 name a0 = jsgf name [a0]
{-# INLINE jsg1 #-}

-- | Handy way to call a function that expects two arguments
jsg2 :: (ToJSString name, ToJSVal a0, ToJSVal a1) => name -> a0 -> a1 -> JSM JSVal
jsg2 name a0 a1 = jsgf name (a0, a1)
{-# INLINE jsg2 #-}

-- | Handy way to call a function that expects three arguments
jsg3 :: (ToJSString name, ToJSVal a0, ToJSVal a1, ToJSVal a2)
    => name -> a0 -> a1 -> a2 -> JSM JSVal
jsg3 name a0 a1 a2 = jsgf name (a0, a1, a2)
{-# INLINE jsg3 #-}

-- | Handy way to call a function that expects four arguments
jsg4 :: (ToJSString name, ToJSVal a0, ToJSVal a1, ToJSVal a2,
        ToJSVal a3)
    => name -> a0 -> a1 -> a2 -> a3 -> JSM JSVal
jsg4 name a0 a1 a2 a3 = jsgf name (a0, a1, a2, a3)
{-# INLINE jsg4 #-}

-- | Handy way to call a function that expects five arguments
jsg5 :: (ToJSString name, ToJSVal a0, ToJSVal a1, ToJSVal a2,
        ToJSVal a3, ToJSVal a4)
    => name -> a0 -> a1 -> a2 -> a3 -> a4 -> JSM JSVal
jsg5 name a0 a1 a2 a3 a4 = jsgf name (a0, a1, a2, a3, a4)
{-# INLINE jsg5 #-}

-- | Call a JavaScript function
--
-- >>> testJSaddle $ eval "'Hello World'.indexOf('World')"
-- >>> testJSaddle $ val "Hello World" # "indexOf" $ ["World"]
-- 6
infixr 2 #
(#) :: (MakeObject this, ToJSString name, MakeArgs args)
    => this -> name -> args -> JSM JSVal
(#) this name args = do
    rthis <- makeObject this
    f <- rethrow $ objGetPropertyByName rthis name
    f' <- valToObject f
    rethrow $ objCallAsFunction f' rthis args
{-# INLINE (#) #-}

-- | Call a JavaScript function
--
-- >>> testJSaddle $ eval "something[6]('World')"
-- >>> testJSaddle $ val something ## 6 ["World"]
infixr 2 ##
(##) :: (MakeObject this, MakeArgs args)
    => this -> Index -> args -> JSM JSVal
(##) this index args = do
    rthis <- makeObject this
    f <- rethrow $ objGetPropertyAtIndex rthis index
    f' <- valToObject f
    rethrow $ objCallAsFunction f' rthis args
{-# INLINE (##) #-}

-- | Set a JavaScript property
--
-- >>> testJSaddle $ eval "var j = {}; j.x = 1; j.x"
-- >>> testJSaddle $ do {j <- eval "({})"; j <# "x" 1; j!"x"}
-- 1
infixr 1 <#
(<#) :: (MakeObject this, ToJSString name, ToJSVal val)
     => this           -- ^ Object to set the property on
     -> name           -- ^ Name of the property to set
     -> val            -- ^ Value to set it to
     -> JSM ()
(<#) this name val = do
    rthis <- makeObject this
    rethrow $ objSetPropertyByName rthis name val 0
{-# INLINE (<#) #-}

-- | Set a JavaScript property
--
-- >>> testJSaddle $ eval "var j = {}; j[6] = 1; j[6]"
-- >>> testJSaddle $ do {j <- eval "({})"; j <## 6 1; j!!6}
-- 1
infixr 1 <##
(<##) :: (MakeObject this, ToJSVal val)
     => this           -- ^ Object to set the property on
     -> Index          -- ^ Index of the property to set
     -> val            -- ^ Value to set it to
     -> JSM ()
(<##) this index val = do
    rthis <- makeObject this
    rethrow $ objSetPropertyAtIndex rthis index val
{-# INLINE (<##) #-}

-- | Use this to create a new JavaScript object
--
-- If you pass more than 7 arguments to a constructor for a built in
-- JavaScript type (like Date) then this function will fail.
--
-- >>> testJSaddle $ new (jsg "Date") (2013, 1, 1)
-- Fri Feb 01 2013 00:00:00 GMT+1300 (NZDT)
new :: (MakeObject constructor, MakeArgs args)
    => constructor
    -> args
    -> JSM JSVal
new constructor args = do
    f <- makeObject constructor
    rethrow $ objCallAsConstructor f args
{-# INLINE new #-}

-- | Call function with a given @this@.  In most cases you should use '#'.
--
-- >>> testJSaddle $ eval "(function(){return this;}).apply('Hello', [])"
-- >>> testJSaddle $ do { test <- eval "(function(){return this;})"; call test (val "Hello") () }
-- Hello
call :: (MakeObject f, MakeObject this, MakeArgs args)
    => f -> this -> args -> JSM JSVal
call f this args = do
    rfunction <- makeObject f
    rthis     <- makeObject this
    rethrow $ objCallAsFunction rfunction rthis args
{-# INLINE call #-}

-- | Make an empty object using the default constuctor
--
-- >>> testJSaddle $ eval "var a = {}; a.x = 'Hello'; a.x"
-- >>> testJSaddle $ do { a <- obj; a ^. js "x" <# "Hello"; a ^. js "x" }
-- Hello
obj :: JSM Object
#ifdef ghcjs_HOST_OS
obj = liftIO Object.create
#else
obj = do
    gctxt <- ask
    Object <$> (liftIO (jsobjectmake gctxt nullPtr nullPtr) >>= makeNewJSVal)
#endif
{-# INLINE obj #-}

-- | Type used for Haskell functions called from JavaScript.
type JSCallAsFunction = JSVal      -- ^ Function object
                     -> JSVal      -- ^ this
                     -> [JSVal]    -- ^ Function arguments
                     -> JSM JSUndefined -- ^ Only 'JSUndefined' can be returned because
                                        --   the function may need to be executed in a
                                        --   different thread.  If you need to get a
                                        --   value out pass in a continuation function
                                        --   as an argument and invoke it from haskell.

-- | Short hand @::JSCallAsFunction@ so a haskell function can be passed to
--   a to a JavaScipt one.
--
-- >>> testJSaddle $ eval "(function(f) {f('Hello');})(function (a) {console.log(a)})"
-- >>> testJSaddle $ call (eval "(function(f) {f('Hello');})") global [fun $ \ _ _ args -> valToText (head args) >>= (liftIO . putStrLn . T.unpack) ]
-- Hello
-- undefined
fun :: JSCallAsFunction -> JSCallAsFunction
fun = id
{-# INLINE fun #-}

#if !defined(ghcjs_HOST_OS)
foreign import ccall "wrapper"
  mkJSObjectCallAsFunctionCallback :: JSObjectCallAsFunctionCallback' -> IO JSObjectCallAsFunctionCallback
#endif

#ifdef ghcjs_HOST_OS
type HaskellCallback = Callback (JSVal -> JSVal -> IO ())
#else
type HaskellCallback = JSObjectCallAsFunctionCallback
#endif

data Function = Function {functionCallback :: HaskellCallback, functionObject :: Object}

-- ^ Make a JavaScript function object that wraps a Haskell function.
function :: ToJSString name
         => name             -- ^ Name of the function
         -> JSCallAsFunction -- ^ Haskell function to call
         -> JSM Function       -- ^ Returns a JavaScript function object that will
                             --   call the Haskell one when it is called
#ifdef ghcjs_HOST_OS
function name f = liftIO $ do
    callback <- syncCallback2 ContinueAsync $ \this args -> do
        rargs <- Array.toListIO (coerce args)
        runReaderT (f this this rargs) () -- TODO pass function object through
    Function callback <$> makeFunctionWithCallback (toJSString name) callback
foreign import javascript unsafe "$r = function () { $2(this, arguments); }"
    makeFunctionWithCallback :: JSString -> Callback (JSVal -> JSVal -> IO ()) -> IO Object
#else
function name f = do
    gctxt <- ask
    callback <- liftIO $ mkJSObjectCallAsFunctionCallback (wrap gctxt)
    withJSString (toJSString name) $ \name' ->
        Function callback . Object <$>
            (liftIO (jsobjectmakefunctionwithcallback gctxt name' callback) >>= makeNewJSVal)
  where
    wrap gctxt _ctx fobj' this' argc argv exception = do
            args' <- peekArray (fromIntegral argc) argv
            (`runReaderT` gctxt) $ do
                fobj <- makeNewJSVal fobj'
                this <- makeNewJSVal this'
                args <- mapM makeNewJSVal args'
                f fobj this args
                liftIO $ jsvaluemakeundefined gctxt
      `E.catch` \(e :: SomeException) ->
            (`runReaderT` gctxt) $ do
                withToJSVal (show e) $ liftIO . poke exception
                liftIO $ jsvaluemakeundefined gctxt
#endif

freeFunction :: MonadIO m => Function -> m ()
freeFunction (Function callback _) = liftIO $
#ifdef ghcjs_HOST_OS
    releaseCallback callback
#else
    freeHaskellFunPtr callback
#endif

instance ToJSVal Function where
    toJSVal (Function _ f) = toJSVal f
    {-# INLINE toJSVal #-}

-- | A callback to Haskell can be used as a JavaScript value.  This will create
--   an anonymous JavaScript function object.  Use 'function' to create one with
--   a name.
instance ToJSVal JSCallAsFunction where
    toJSVal f = functionObject <$> function nullJSString f >>= toJSVal
    {-# INLINE toJSVal #-}

instance MakeArgs JSCallAsFunction where
    makeArgs f = do
        rarg <- functionObject <$> function nullJSString f >>= toJSVal
        return [rarg]
    {-# INLINE makeArgs #-}

makeArray :: MakeArgs args => args -> MutableJSArray -> JSM Object
#ifdef ghcjs_HOST_OS
makeArray args exceptions = do
    rargs <- makeArgs args
    liftIO $ Object . jsval <$> Array.fromListIO rargs
#else
makeArray args exceptions = do
    gctxt <- ask
    rargs <- makeArgs args
    result <-
        withJSVals rargs $ \rargs' ->
            liftIO $ withArrayLen rargs' $ \ len ptr ->
                jsobjectmakearray gctxt (fromIntegral len) ptr exceptions
    Object <$> makeNewJSVal result
#endif
{-# INLINE makeArray #-}

-- | Make an JavaScript array from a list of values
--
-- >>> testJSaddle $ eval "['Hello', 'World'][1]"
-- >>> testJSaddle $ array ["Hello", "World"] !! 1
-- World
-- >>> testJSaddle $ eval "['Hello', null, undefined, true, 1]"
-- >>> testJSaddle $ array ("Hello", JSNull, (), True, 1.0::Double)
-- Hello,,,true,1
array :: MakeArgs args => args -> JSM Object
array = rethrow . makeArray

-- Make an array out of various lists
instance ToJSVal [JSVal] where
    toJSVal = toJSVal . array
    {-# INLINE toJSVal #-}

instance ToJSVal [JSM JSVal] where
    toJSVal = toJSVal . array
    {-# INLINE toJSVal #-}

instance ToJSVal [Double] where
    toJSVal = toJSVal . array
    {-# INLINE toJSVal #-}

instance ToJSVal [Float] where
    toJSVal = toJSVal . array
    {-# INLINE toJSVal #-}

instance ToJSVal [Int] where
    toJSVal = toJSVal . array
    {-# INLINE toJSVal #-}

instance ToJSVal [JSString] where
    toJSVal = toJSVal . array
    {-# INLINE toJSVal #-}

instance ToJSVal [String] where
    toJSVal = toJSVal . array
    {-# INLINE toJSVal #-}

instance ToJSVal [Text] where
    toJSVal = toJSVal . array
    {-# INLINE toJSVal #-}

instance ToJSVal [Bool] where
    toJSVal = toJSVal . array
    {-# INLINE toJSVal #-}

-- | JavaScript's global object
global :: JSM Object
#ifdef ghcjs_HOST_OS
global = liftIO js_window
{-# INLINE global #-}
foreign import javascript unsafe "$r = window"
    js_window :: IO Object
#else
global = do
    gctxt <- ask
    result <- liftIO $ jscontextgetglobalobject gctxt
    Object <$> makeNewJSVal result
{-# INLINE global #-}
#endif

-- | Get an array containing the property names present on a given object
#if !defined(ghcjs_HOST_OS)
copyPropertyNames :: MakeObject this => this -> JSM JSPropertyNameArray
copyPropertyNames this = do
    gctxt <- ask
    this' <- makeObject this
    withObject this' $ \rthis ->
        liftIO $ jsobjectcopypropertynames gctxt rthis
{-# INLINE copyPropertyNames #-}

-- | Get the number of names in a property name array
propertyNamesCount :: MonadIO m => JSPropertyNameArray -> m CSize
propertyNamesCount names = liftIO $ jspropertynamearraygetcount names
{-# INLINE propertyNamesCount #-}

-- | Get a name out of a property name array
propertyNamesAt :: MonadIO m => JSPropertyNameArray -> CSize -> m JSString
propertyNamesAt names index = liftIO $ jspropertynamearraygetnameatindex names index >>= wrapJSString
{-# INLINE propertyNamesAt #-}

-- | Convert property array to a list
propertyNamesList :: MonadIO m => JSPropertyNameArray -> m [JSString]
propertyNamesList names = do
    count <- propertyNamesCount names
    mapM (propertyNamesAt names) $ enumFromTo 0 (count - 1)
{-# INLINE propertyNamesList #-}
#endif

-- | Get a list containing the property names present on a given object
propertyNames :: MakeObject this => this -> JSM [JSString]
#ifdef ghcjs_HOST_OS
propertyNames this = makeObject this >>= liftIO . js_propertyNames >>= liftIO . (fmap (map pFromJSVal)) . Array.toListIO
{-# INLINE propertyNames #-}
foreign import javascript unsafe "$r = []; h$forIn($1, function(n){$r.push(n);})"
    js_propertyNames :: Object -> IO JSArray
#else
propertyNames this = copyPropertyNames this >>= propertyNamesList
{-# INLINE propertyNames #-}
#endif

-- | Get a list containing references to all the  properties present on a given object
properties :: MakeObject this => this -> JSM [JSVal]
properties this = propertyNames this >>= mapM (this !)

-- | Call a JavaScript object as function.  Consider using '#'.
objCallAsFunction :: MakeArgs args
                  => Object
                  -> Object
                  -> args
                  -> MutableJSArray
                  -> JSM JSVal
#ifdef ghcjs_HOST_OS
objCallAsFunction f this args exceptions = do
    rargs <- makeArgs args >>= liftIO . Array.fromListIO
    liftIO $ js_apply f this rargs exceptions
{-# INLINE objCallAsFunction #-}
foreign import javascript unsafe "try { $r = $1.apply($2, $3) } catch(e) { $4[0] = e }"
    js_apply :: Object -> Object -> MutableJSArray -> MutableJSArray -> IO JSVal
#else
objCallAsFunction f this args exceptions = do
    gctxt <- ask
    rargs <- makeArgs args
    result <-
        withObject f $ \rfunction ->
            withObject this $ \rthis ->
                withJSVals rargs $ \rargs' ->
                    liftIO $ withArrayLen rargs' $ \ largs pargs ->
                        jsobjectcallasfunction gctxt rfunction rthis (fromIntegral largs) pargs exceptions
    makeNewJSVal result
{-# INLINE objCallAsFunction #-}
#endif

-- | Call a JavaScript object as a constructor. Consider using 'new'.
--
-- If you pass more than 7 arguments to a constructor for a built in
-- JavaScript type (like Date) then this function will fail.
objCallAsConstructor :: MakeArgs args
                     => Object
                     -> args
                     -> MutableJSArray
                     -> JSM JSVal
#ifdef ghcjs_HOST_OS
objCallAsConstructor f args exceptions = do
    rargs <- makeArgs args >>= liftIO . Array.fromListIO
    liftIO $ js_new f rargs exceptions
foreign import javascript unsafe "\
    try {\
        switch($2.length) {\
            case 0 : $r = new $1(); break;\
            case 1 : $r = new $1($2[0]); break;\
            case 2 : $r = new $1($2[0],$2[1]); break;\
            case 3 : $r = new $1($2[0],$2[1],$2[2]); break;\
            case 4 : $r = new $1($2[0],$2[1],$2[2],$2[3]); break;\
            case 5 : $r = new $1($2[0],$2[1],$2[2],$2[3],$2[4]); break;\
            case 6 : $r = new $1($2[0],$2[1],$2[2],$2[3],$2[4],$2[5]); break;\
            case 7 : $r = new $1($2[0],$2[1],$2[2],$2[3],$2[4],$2[5],$2[6]); break;\
            default:\
                var ret;\
                var temp = function() {\
                    ret = $1.apply(this, $2);\
                };\
                temp.prototype = $1.prototype;\
                var i = new temp();\
                if(ret instanceof Object)\
                    return ret;\
                i.constructor = $1;\
                return i;\
        }\
    }\
    catch(e) {\
        $3[0] = e;\
    }"
    js_new :: Object -> MutableJSArray -> MutableJSArray -> IO JSVal
#else
objCallAsConstructor f args exceptions = do
    gctxt <- ask
    rargs <- makeArgs args
    result <-
        withObject f $ \rfunction ->
            withJSVals rargs $ \rargs' ->
                liftIO $ withArrayLen rargs' $ \ largs pargs ->
                    jsobjectcallasconstructor gctxt rfunction (fromIntegral largs) pargs exceptions
    makeNewJSVal result
#endif

nullObject :: Object
#ifdef ghcjs_HOST_OS
nullObject = Object nullRef
#else
nullObject = Object . unsafePerformIO $ newForeignPtr_ nullPtr
#endif




