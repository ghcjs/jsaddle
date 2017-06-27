{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# OPTIONS_GHC -Wno-dodgy-exports -Wno-dodgy-imports #-}
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
    Object(..)
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
  , create
  , getProp
  , unsafeGetProp
  , setProp
  , unsafeSetProp

  -- * Calling Haskell From JavaScript
  , Function(..)
  , function
  , asyncFunction
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
  , fromListIO
  , array

  -- * Global Object
  , global

  -- * Enumerating Properties
  , listProps
  , propertyNames
  , properties

  -- * Low level
  , objCallAsFunction
  , objCallAsConstructor
  , nullObject
) where

import Prelude hiding ((!!))
import Data.Coerce (coerce)
#ifdef ghcjs_HOST_OS
import GHCJS.Types (nullRef)
import GHCJS.Foreign.Callback
       (releaseCallback, syncCallback2, asyncCallback2, OnBlocked(..), Callback)
import GHCJS.Marshal (ToJSVal(..))
import JavaScript.Array (MutableJSArray)
import qualified JavaScript.Array as Array (toListIO, fromListIO)
import JavaScript.Array.Internal (SomeJSArray(..))
import JavaScript.Object (create, listProps)
import Language.Javascript.JSaddle.Monad (JSM)
import Language.Javascript.JSaddle.Types
       (JSString, Object(..),
        JSVal(..), JSCallAsFunction)
#else
import GHCJS.Marshal.Internal (ToJSVal(..))
import Language.Javascript.JSaddle.Native
       (newAsyncCallback, newSyncCallback, callAsFunction, callAsConstructor)
import Language.Javascript.JSaddle.Monad (askJSM, JSM)
import Language.Javascript.JSaddle.Types
       (JSValueForSend(..), AsyncCommand(..), JSString, Object(..),
        SomeJSArray(..), JSVal(..), JSCallAsFunction, JSContextRef(..))
import JavaScript.Object.Internal (create, listProps)
import Language.Javascript.JSaddle.Run (sendAsyncCommand)
#endif
import JavaScript.Array.Internal (fromListIO)
import Language.Javascript.JSaddle.Value (valToObject)
import Language.Javascript.JSaddle.Classes (MakeObject(..))
import Language.Javascript.JSaddle.Marshal.String (ToJSString(..))
import Language.Javascript.JSaddle.Arguments (MakeArgs(..))
import Control.Monad.IO.Class (MonadIO(..))
import Language.Javascript.JSaddle.Properties
import Control.Lens (IndexPreservingGetter, to)

-- $setup
-- >>> import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
-- >>> import Language.Javascript.JSaddle.Test (testJSaddle)
-- >>> import Language.Javascript.JSaddle.Evaluate (eval)
-- >>> import Language.Javascript.JSaddle.Value (val, valToText, JSNull(..), deRefVal)
-- >>> import Language.Javascript.JSaddle.String (strToText)
-- >>> import Control.Lens.Operators ((^.))
-- >>> import qualified Data.Text as T (unpack)

-- | Object can be made by evaluating a fnction in 'JSM' as long
--   as it returns something we can make into a Object.
instance MakeObject v => MakeObject (JSM v) where
    makeObject v = v >>= makeObject

-- | Lookup a property based on its name.
--
-- >>> testJSaddle $ eval "'Hello World'.length"
-- 11
-- >>> testJSaddle $ val "Hello World" ! "length"
-- 11
(!) :: (MakeObject this, ToJSString name)
    => this           -- ^ Object to look on
    -> name           -- ^ Name of the property to find
    -> JSM JSVal -- ^ Property reference
this ! name = do
    rthis <- makeObject this
    objGetPropertyByName rthis name

-- | Lookup a property based on its index.
--
-- >>> testJSaddle $ eval "'Hello World'[6]"
-- W
-- >>> testJSaddle $ val "Hello World" !! 6
-- W
(!!) :: (MakeObject this)
     => this      -- ^ Object to look on
     -> Int       -- ^ Index of the property to lookup
     -> JSM JSVal -- ^ Property reference
this !! index = do
    rthis <- makeObject this
    objGetPropertyAtIndex rthis index

-- | Makes a getter for a particular property name.
--
-- > js name = to (!name)
--
-- >>> testJSaddle $ eval "'Hello World'.length"
-- 11
-- >>> testJSaddle $ val "Hello World" ^. js "length"
-- 11
js :: (MakeObject s, ToJSString name)
   => name          -- ^ Name of the property to find
   -> IndexPreservingGetter s (JSM JSVal)
js name = to (!name)

-- | Makes a setter for a particular property name.
--
-- > jss name = to (<#name)
--
-- >>> testJSaddle $ eval "'Hello World'.length = 12"
-- 12
-- >>> testJSaddle $ val "Hello World" ^. jss "length" 12
-- undefined
jss :: (ToJSString name, ToJSVal val)
   => name          -- ^ Name of the property to find
   -> val
   -> forall o . MakeObject o => IndexPreservingGetter o (JSM ())
jss name val = to (\o -> o <# name $ val)

-- | Handy way to call a function
--
-- > jsf name = to (\o -> o # name $ args)
--
-- >>> testJSaddle $ val "Hello World" ^. jsf "indexOf" ["World"]
-- 6
jsf :: (ToJSString name, MakeArgs args) => name -> args -> JSF
jsf name args = to (\o -> o # name $ args)

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

-- | Handy way to call a function that expects one argument
--
-- > js1 name a0 = jsf name [a0]
--
-- >>> testJSaddle $ val "Hello World" ^. js1 "indexOf" "World"
-- 6
js1 :: (ToJSString name, ToJSVal a0) => name -> a0 -> JSF
js1 name a0 = jsf name [a0]

-- | Handy way to call a function that expects two arguments
js2 :: (ToJSString name, ToJSVal a0, ToJSVal a1) => name -> a0 -> a1 -> JSF
js2 name a0 a1 = jsf name (a0, a1)

-- | Handy way to call a function that expects three arguments
js3 :: (ToJSString name, ToJSVal a0, ToJSVal a1, ToJSVal a2)
    => name -> a0 -> a1 -> a2 -> JSF
js3 name a0 a1 a2 = jsf name (a0, a1, a2)

-- | Handy way to call a function that expects four arguments
js4 :: (ToJSString name, ToJSVal a0, ToJSVal a1, ToJSVal a2,
        ToJSVal a3)
    => name -> a0 -> a1 -> a2 -> a3 -> JSF
js4 name a0 a1 a2 a3 = jsf name (a0, a1, a2, a3)

-- | Handy way to call a function that expects five arguments
js5 :: (ToJSString name, ToJSVal a0, ToJSVal a1, ToJSVal a2,
        ToJSVal a3, ToJSVal a4)
    => name -> a0 -> a1 -> a2 -> a3 -> a4 -> JSF
js5 name a0 a1 a2 a3 a4 = jsf name (a0, a1, a2, a3, a4)


-- | Handy way to get and hold onto a reference top level javascript
--
-- >>> testJSaddle $ eval "w = console; w.log('Hello World')"
-- undefined
-- >>> testJSaddle $ do w <- jsg "console"; w ^. js1 "log" "Hello World"
-- undefined
jsg :: ToJSString a => a -> JSM JSVal
jsg name = global ! name

-- | Handy way to call a function
--
-- > jsgf name = jsg name . to (# args)
--
-- >>> testJSaddle $ eval "globalFunc = function (x) {return x.length;}"
-- function (x) {return x.length;}
-- >>> testJSaddle $ jsgf "globalFunc" ["World"]
-- 5
jsgf :: (ToJSString name, MakeArgs args) => name -> args -> JSM JSVal
jsgf name = global # name

-- | Handy way to call a function that expects no arguments
--
-- > jsg0 name = jsgf name ()
--
-- >>> testJSaddle $ jsg0 "globalFunc" >>= valToText
-- A JavaScript exception was thrown! (may not reach Haskell code)
-- TypeError:...undefine...
jsg0 :: (ToJSString name) => name -> JSM JSVal
jsg0 name = jsgf name ()

-- | Handy way to call a function that expects one argument
--
-- > jsg1 name a0 = jsgf name [a0]
--
-- >>> testJSaddle $ jsg1 "globalFunc" "World"
-- 5
jsg1 :: (ToJSString name, ToJSVal a0) => name -> a0 -> JSM JSVal
jsg1 name a0 = jsgf name [a0]

-- | Handy way to call a function that expects two arguments
jsg2 :: (ToJSString name, ToJSVal a0, ToJSVal a1) => name -> a0 -> a1 -> JSM JSVal
jsg2 name a0 a1 = jsgf name (a0, a1)

-- | Handy way to call a function that expects three arguments
jsg3 :: (ToJSString name, ToJSVal a0, ToJSVal a1, ToJSVal a2)
    => name -> a0 -> a1 -> a2 -> JSM JSVal
jsg3 name a0 a1 a2 = jsgf name (a0, a1, a2)

-- | Handy way to call a function that expects four arguments
jsg4 :: (ToJSString name, ToJSVal a0, ToJSVal a1, ToJSVal a2,
        ToJSVal a3)
    => name -> a0 -> a1 -> a2 -> a3 -> JSM JSVal
jsg4 name a0 a1 a2 a3 = jsgf name (a0, a1, a2, a3)

-- | Handy way to call a function that expects five arguments
jsg5 :: (ToJSString name, ToJSVal a0, ToJSVal a1, ToJSVal a2,
        ToJSVal a3, ToJSVal a4)
    => name -> a0 -> a1 -> a2 -> a3 -> a4 -> JSM JSVal
jsg5 name a0 a1 a2 a3 a4 = jsgf name (a0, a1, a2, a3, a4)

-- | Call a JavaScript function
--
-- >>> testJSaddle $ eval "'Hello World'.indexOf('World')"
-- 6
-- >>> testJSaddle $ val "Hello World" # "indexOf" $ ["World"]
-- 6
infixr 2 #
(#) :: (MakeObject this, ToJSString name, MakeArgs args)
    => this -> name -> args -> JSM JSVal
(#) this name args = do
    rthis <- makeObject this
    f <- objGetPropertyByName rthis name
    f' <- valToObject f
    objCallAsFunction f' rthis args

-- | Call a JavaScript function
--
-- >>> testJSaddle $ eval "something = {}; something[6]=function (x) {return x.length;}; something[6]('World')"
-- 5
-- >>> testJSaddle $ jsg "something" ## 6 $ ["World"]
-- 5
infixr 2 ##
(##) :: (MakeObject this, MakeArgs args)
    => this -> Int -> args -> JSM JSVal
(##) this index args = do
    rthis <- makeObject this
    f <- objGetPropertyAtIndex rthis index
    f' <- valToObject f
    objCallAsFunction f' rthis args

-- | Set a JavaScript property
--
-- >>> testJSaddle $ eval "var j = {}; j.x = 1; j.x"
-- 1
-- >>> testJSaddle $ do {j <- obj; (j <# "x") 1; j!"x"}
-- 1
infixr 1 <#
(<#) :: (MakeObject this, ToJSString name, ToJSVal val)
     => this           -- ^ Object to set the property on
     -> name           -- ^ Name of the property to set
     -> val            -- ^ Value to set it to
     -> JSM ()
(<#) this name val = do
    rthis <- makeObject this
    objSetPropertyByName rthis name val

-- | Set a JavaScript property
--
-- >>> testJSaddle $ eval "var j = {}; j[6] = 1; j[6]"
-- 1
-- >>> testJSaddle $ do {j <- obj; (j <## 6) 1; j!!6}
-- 1
infixr 1 <##
(<##) :: (MakeObject this, ToJSVal val)
     => this   -- ^ Object to set the property on
     -> Int    -- ^ Index of the property to set
     -> val    -- ^ Value to set it to
     -> JSM ()
(<##) this index val = do
    rthis <- makeObject this
    objSetPropertyAtIndex rthis index val

-- | Use this to create a new JavaScript object
--
-- If you pass more than 7 arguments to a constructor for a built in
-- JavaScript type (like Date) then this function will fail.
--
-- >>> testJSaddle $ new (jsg "Date") (2013, 1, 1)
-- Fri Feb 01 2013 00:00:00 GMT+... (...)
new :: (MakeObject constructor, MakeArgs args)
    => constructor
    -> args
    -> JSM JSVal
new constructor args = do
    f <- makeObject constructor
    objCallAsConstructor f args

-- | Call function with a given @this@.  In most cases you should use '#'.
--
-- >>> testJSaddle $ eval "(function(){return this;}).apply('Hello', [])"
-- Hello
-- >>> testJSaddle $ do { test <- eval "(function(){return this;})"; call test (val "Hello") () }
-- Hello
call :: (MakeObject f, MakeObject this, MakeArgs args)
    => f -> this -> args -> JSM JSVal
call f this args = do
    rfunction <- makeObject f
    rthis     <- makeObject this
    objCallAsFunction rfunction rthis args

-- | Make an empty object using the default constuctor
--
-- >>> testJSaddle $ eval "var a = {}; a.x = 'Hello'; a.x"
-- Hello
-- >>> testJSaddle $ do { a <- obj; (a <# "x") "Hello"; a ^. js "x" }
-- Hello
obj :: JSM Object
obj = create

-- | Short hand @::JSCallAsFunction@ so a haskell function can be passed to
--   a to a JavaScipt one.
--
-- >>> testJSaddle $ eval "(function(f) {f('Hello');})(function (a) {console.log(a)})"
-- undefined
-- >>> :{
--  testJSaddle $ do
--    result <- liftIO newEmptyMVar
--    deRefVal $ call (eval "(function(f) {f('Hello');})") global [fun $ \ _ _ [arg1] -> do
--         valToText arg1 >>= (liftIO . putMVar result)
--         ]
--    liftIO $ takeMVar result
-- :}
-- Hello
fun :: JSCallAsFunction -> JSCallAsFunction
fun = id

#ifdef ghcjs_HOST_OS
data Function = Function {functionCallback :: Callback (JSVal -> JSVal -> IO ()), functionObject :: Object}
#else
newtype Function = Function {functionObject :: Object}
#endif


#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "$r = function () { $1(this, arguments); }"
    makeFunctionWithCallback :: Callback (JSVal -> JSVal -> IO ()) -> IO Object
#endif

-- ^ Make a JavaScript function object that wraps a Haskell function.
-- Calls made to the function will be synchronous where possible
-- (on GHCJS it uses on `syncCallback2` with `ContinueAsync`).
function :: JSCallAsFunction -- ^ Haskell function to call
         -> JSM Function     -- ^ Returns a JavaScript function object that will
                             --   call the Haskell one when it is called
#ifdef ghcjs_HOST_OS
function f = do
    callback <- syncCallback2 ContinueAsync $ \this args -> do
        rargs <- Array.toListIO (coerce args)
        f this this rargs -- TODO pass function object through
    Function callback <$> makeFunctionWithCallback callback
#else
function f = do
    object <- newSyncCallback f
    return $ Function object
#endif

-- ^ Make a JavaScript function object that wraps a Haskell function.
-- Calls made to the function will be Asynchronous.
asyncFunction :: JSCallAsFunction -- ^ Haskell function to call
              -> JSM Function     -- ^ Returns a JavaScript function object that will
                                  --   call the Haskell one when it is called
#ifdef ghcjs_HOST_OS
asyncFunction f = do
    callback <- asyncCallback2 $ \this args -> do
        rargs <- Array.toListIO (coerce args)
        f this this rargs -- TODO pass function object through
    Function callback <$> makeFunctionWithCallback callback
#else
asyncFunction f = do
    object <- newAsyncCallback f
    return $ Function object
#endif

freeFunction :: Function -> JSM ()
#ifdef ghcjs_HOST_OS
freeFunction (Function callback _) = liftIO $
    releaseCallback callback
#else
freeFunction (Function (Object (JSVal objectRef))) =
    sendAsyncCommand (FreeCallback (JSValueForSend objectRef))
#endif

instance ToJSVal Function where
    toJSVal = toJSVal . functionObject

-- | A callback to Haskell can be used as a JavaScript value.  This will create
--   an anonymous JavaScript function object.  Use 'function' to create one with
--   a name.
instance ToJSVal JSCallAsFunction where
    toJSVal f = functionObject <$> function f >>= toJSVal

instance MakeArgs JSCallAsFunction where
    makeArgs f = do
        rarg <- functionObject <$> function f >>= toJSVal
        return [rarg]

-- | Make an JavaScript array from a list of values
--
-- >>> testJSaddle $ eval "['Hello', 'World'][1]"
-- World
-- >>> testJSaddle $ array ["Hello", "World"] !! 1
-- World
-- >>> testJSaddle $ eval "['Hello', null, undefined, true, 1]"
-- Hello,,,true,1
-- >>> testJSaddle $ array ("Hello", JSNull, (), True, 1.0::Double)
-- Hello,,,true,1
array :: MakeArgs args => args -> JSM Object
array args = do
    rargs <- makeArgs args
    Object . coerce <$> fromListIO rargs

-- | JavaScript's global object
global :: Object
#ifdef ghcjs_HOST_OS
global = js_window
foreign import javascript unsafe "$r = window"
    js_window :: Object
#else
global = Object (JSVal 4)
#endif

-- | Get a list containing the property names present on a given object
-- >>> testJSaddle $ show . map strToText <$> propertyNames obj
-- []
-- >>> testJSaddle $ show . map strToText <$> propertyNames (eval "({x:1, y:2})")
-- ["x","y"]
propertyNames :: MakeObject this => this -> JSM [JSString]
propertyNames this = makeObject this >>= listProps

-- | Get a list containing references to all the  properties present on a given object
properties :: MakeObject this => this -> JSM [JSVal]
properties this = propertyNames this >>= mapM (this !)

-- | Call a JavaScript object as function.  Consider using '#'.
objCallAsFunction :: MakeArgs args
                  => Object
                  -> Object
                  -> args
                  -> JSM JSVal
#ifdef ghcjs_HOST_OS
objCallAsFunction f this args = do
    rargs <- makeArgs args >>= liftIO . Array.fromListIO
    liftIO $ js_apply f this rargs
foreign import javascript unsafe "$r = $1.apply($2, $3)"
    js_apply :: Object -> Object -> MutableJSArray -> IO JSVal
#else
objCallAsFunction f this args = do
    rargs <- makeArgs args
    callAsFunction f this rargs
#endif

-- | Call a JavaScript object as a constructor. Consider using 'new'.
--
-- If you pass more than 7 arguments to a constructor for a built in
-- JavaScript type (like Date) then this function will fail.
objCallAsConstructor :: MakeArgs args
                     => Object
                     -> args
                     -> JSM JSVal
#ifdef ghcjs_HOST_OS
objCallAsConstructor f args = do
    rargs <- makeArgs args >>= liftIO . Array.fromListIO
    liftIO $ js_new f rargs
foreign import javascript unsafe "\
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
                var temp = function() {\
                    ret = $1.apply(this, $2);\
                };\
                temp.prototype = $1.prototype;\
                var i = new temp();\
                if(ret instanceof Object) {\
                    $r = ret;\
                } else {\
                    i.constructor = $1;\
                    $r = i;\
                }\
        }"
    js_new :: Object -> MutableJSArray -> IO JSVal
#else
objCallAsConstructor f args = do
    rargs <- makeArgs args
    callAsConstructor f rargs
#endif

-- >>> testJSaddle $ strictEqual nullObject (eval "null")
-- true
nullObject :: Object
#ifdef ghcjs_HOST_OS
nullObject = Object nullRef
#else
nullObject = Object (JSVal 0)
#endif
