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
-- | Interface to JavaScript array
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Array
    ( SomeJSArray(..)
    , JSArray
    , MutableJSArray
    , create
    , length
    , lengthIO
    , nullIO
    , fromListIO
    , toListIO
    , read
    , write
    , append
    , push
    , pop
    , unshift
    , shift
    , reverse
    , takeIO
    , dropIO
    , sliceIO
    , freeze
    , unsafeFreeze
    , thaw
    , unsafeThaw
    ) where

import Prelude hiding (read, reverse, null)
import Control.Monad (void)
import Language.Javascript.JSaddle.Types
       (JSM, JSVal, SomeJSArray(..), JSArray,
        MutableJSArray)
import Control.Lens.Operators ((^.))
import Language.Javascript.JSaddle.Object (js2, js0, (##), js1, js)
import Language.Javascript.JSaddle.Value (valToNumber)
import JavaScript.Array.Internal (create, fromListIO, toListIO, read, push)

lengthIO :: SomeJSArray m -> JSM Int
lengthIO (SomeJSArray x) = round <$> (x ^. js "length" >>= valToNumber)
{-# INLINE lengthIO #-}

nullIO :: SomeJSArray m -> JSM Bool
nullIO = fmap (== 0) . lengthIO
{-# INLINE nullIO #-}

append :: SomeJSArray m -> SomeJSArray m -> JSM (SomeJSArray m1)
append (SomeJSArray x) (SomeJSArray y) = SomeJSArray <$> x ^. js1 "concat" y
{-# INLINE append #-}

write :: Int -> JSVal -> MutableJSArray -> JSM ()
write n e (SomeJSArray x) = void $ (x ## n) e
{-# INLINE write #-}

pop :: MutableJSArray -> JSM JSVal
pop (SomeJSArray x) = x ^. js0 "pop"
{-# INLINE pop #-}

unshift :: JSVal -> MutableJSArray -> JSM ()
unshift e (SomeJSArray x) = void $ x ^. js1 "unshift" e
{-# INLINE unshift #-}

shift :: MutableJSArray -> JSM JSVal
shift (SomeJSArray x) = x ^. js0 "shift"
{-# INLINE shift #-}

reverse :: MutableJSArray -> JSM ()
reverse (SomeJSArray x) = void $ x ^. js0 "reverse"
{-# INLINE reverse #-}

takeIO :: Int -> SomeJSArray m -> JSM (SomeJSArray m1)
takeIO n (SomeJSArray x) = SomeJSArray <$> x ^. js2 "slice" (0::Int) n
{-# INLINE takeIO #-}

dropIO :: Int -> SomeJSArray m -> JSM (SomeJSArray m1)
dropIO n (SomeJSArray x) = SomeJSArray <$> x ^. js1 "slice1" n
{-# INLINE dropIO #-}

sliceIO :: Int -> Int -> JSArray -> JSM (SomeJSArray m1)
sliceIO s n (SomeJSArray x) = SomeJSArray <$> x ^. js2 "slice" s n
{-# INLINE sliceIO #-}

freeze :: MutableJSArray -> JSM JSArray
freeze (SomeJSArray x) = SomeJSArray <$> x ^. js1 "slice" (0::Int)
{-# INLINE freeze #-}

unsafeFreeze :: MutableJSArray -> JSM JSArray
unsafeFreeze (SomeJSArray x) = pure (SomeJSArray x)
{-# INLINE unsafeFreeze #-}

thaw :: JSArray -> JSM MutableJSArray
thaw (SomeJSArray x) = SomeJSArray <$> x ^. js1 "slice" (0::Int)
{-# INLINE thaw #-}

unsafeThaw :: JSArray -> JSM MutableJSArray
unsafeThaw (SomeJSArray x) = pure (SomeJSArray x)
{-# INLINE unsafeThaw #-}


