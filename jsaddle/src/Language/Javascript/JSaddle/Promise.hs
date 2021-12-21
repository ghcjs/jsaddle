{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
#endif
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Promise
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | Interface to JavaScript object
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Promise (
    PromiseFunction(..)
) where

import Language.Javascript.JSaddle.Types
       (JSM, JSVal)
#ifdef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Object (call, fun)
#else
import Language.Javascript.JSaddle.Object (jsg1, call, fun)
#endif
import Control.Monad (void)
import GHCJS.Marshal.Internal (FromJSVal(..), ToJSVal(..))

#ifdef ghcjs_HOST_OS
foreign import javascript safe "function() { return new Promise($1); }" js_promiseFunction0 :: JSVal -> IO JSVal
foreign import javascript safe "(function(a) { return new Promise(function(resolutionFunc, rejectionFunc) { $1(a,resolutionFunc,rejectionFunc); }); })" js_promiseFunction1 :: JSVal -> IO JSVal
foreign import javascript safe "(function(a,b) { return new Promise(function(resolutionFunc, rejectionFunc) { $1(a,b,resolutionFunc,rejectionFunc); }); })" js_promiseFunction2 :: JSVal -> IO JSVal
foreign import javascript safe "(function(a,b,c) { return new Promise(function(resolutionFunc, rejectionFunc) { $1(a,b,c,resolutionFunc,rejectionFunc); }); })" js_promiseFunction3 :: JSVal -> IO JSVal
foreign import javascript safe "(function(a,b,c,d) { return new Promise(function(resolutionFunc, rejectionFunc) { $1(a,b,c,d,resolutionFunc,rejectionFunc); }); })" js_promiseFunction4 :: JSVal -> IO JSVal
#else
js_promiseFunction0, js_promiseFunction1, js_promiseFunction2, js_promiseFunction3, js_promiseFunction4 :: JSVal -> JSM JSVal
js_promiseFunction0 = jsg1 "h$jsaddle_promiseFunction0"
js_promiseFunction1 = jsg1 "h$jsaddle_promiseFunction1"
js_promiseFunction2 = jsg1 "h$jsaddle_promiseFunction2"
js_promiseFunction3 = jsg1 "h$jsaddle_promiseFunction3"
js_promiseFunction4 = jsg1 "h$jsaddle_promiseFunction4"
#endif

-- $setup
-- >>> import Control.Monad.IO.Class (liftIO)
-- >>> import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
-- >>> import Language.Javascript.JSaddle.Test (testJSaddle)
-- >>> import Language.Javascript.JSaddle.Evaluate (eval)
-- >>> import Language.Javascript.JSaddle.Object (global)
-- >>> import Language.Javascript.JSaddle.Value (val, valToText, JSNull(..), deRefVal)
-- >>> import Language.Javascript.JSaddle.String (strToText)
-- >>> import Control.Lens.Operators ((^.))
-- >>> import qualified Data.Text as T (unpack)

-- | Short hand @::JSCallAsFunction@ so a haskell function can be passed to
--   a to a JavaScipt one.
--
-- >>> :{
--  testJSaddle $ do
--    pf1 <- pfun1 (\arg -> do
--      liftIO . putStrLn $ "pf1 " <> arg
--      return (if null arg then Left "ERROR" else Right arg))
--    pf2 <- pfun1 (\arg -> do
--      liftIO . putStrLn $ "pf2 " <> arg
--      return (if null arg then Left "ERROR" else Right arg))
--    call (eval "(function(pf1, pf2) { pf1('Hello').then(pf2).then(function(r){console.log(r);}); })") global [pf1, pf2]
-- :}
-- HelloX
class PromiseFunction f where
    pfun :: f -> JSM JSVal

instance (ToJSVal l, ToJSVal r) => PromiseFunction (JSM (Either l r)) where
    pfun f = toJSVal (fun $ \_ _ [resolutionFunc, rejectionFunc] ->
        f >>= \case
            Left l -> void $ call rejectionFunc rejectionFunc [l]
            Right r -> void $ call resolutionFunc resolutionFunc [r]) >>= js_promiseFunction0

instance (FromJSVal a, ToJSVal l, ToJSVal r) => PromiseFunction (a -> JSM (Either l r)) where
    pfun f = toJSVal (fun $ \_ _ [a', resolutionFunc, rejectionFunc] -> do
        a <- fromJSValUnchecked a'
        f a >>= \case
            Left l -> void $ call rejectionFunc rejectionFunc [l]
            Right r -> void $ call resolutionFunc resolutionFunc [r]) >>= js_promiseFunction1

pfun1 f = toJSVal (fun $ \_ _ [a', resolutionFunc, rejectionFunc] -> do
        a <- fromJSValUnchecked a'
        f a >>= \case
            Left l -> void $ call rejectionFunc rejectionFunc [l]
            Right r -> void $ call resolutionFunc resolutionFunc [r]) >>= js_promiseFunction1

instance (FromJSVal a, FromJSVal b, ToJSVal l, ToJSVal r) => PromiseFunction (a -> b -> JSM (Either l r)) where
    pfun f = toJSVal (fun $ \_ _ [a', b', resolutionFunc, rejectionFunc] -> do
        a <- fromJSValUnchecked a'
        b <- fromJSValUnchecked b'
        f a b >>= \case
            Left l -> void $ call rejectionFunc rejectionFunc [l]
            Right r -> void $ call resolutionFunc resolutionFunc [r]) >>= js_promiseFunction2

instance (FromJSVal a, FromJSVal b, FromJSVal c, ToJSVal l, ToJSVal r) => PromiseFunction (a -> b -> c -> JSM (Either l r)) where
    pfun f = toJSVal (fun $ \_ _ [a', b', c', resolutionFunc, rejectionFunc] -> do
        a <- fromJSValUnchecked a'
        b <- fromJSValUnchecked b'
        c <- fromJSValUnchecked c'
        f a b c >>= \case
            Left l -> void $ call rejectionFunc rejectionFunc [l]
            Right r -> void $ call resolutionFunc resolutionFunc [r]) >>= js_promiseFunction3

instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, ToJSVal l, ToJSVal r) => PromiseFunction (a -> b -> c -> d -> JSM (Either l r)) where
    pfun f = toJSVal (fun $ \_ _ [a', b', c', d', resolutionFunc, rejectionFunc] -> do
        a <- fromJSValUnchecked a'
        b <- fromJSValUnchecked b'
        c <- fromJSValUnchecked c'
        d <- fromJSValUnchecked d'
        f a b c d >>= \case
            Left l -> void $ call rejectionFunc rejectionFunc [l]
            Right r -> void $ call resolutionFunc resolutionFunc [r]) >>= js_promiseFunction4
