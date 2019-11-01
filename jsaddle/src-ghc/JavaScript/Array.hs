-----------------------------------------------------------------------------
--
-- Module      :  JavaScript.Array
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | Interface to JavaScript array
--
-----------------------------------------------------------------------------
module JavaScript.Array
    ( SomeJSArray(..)
    , JSArray
    , MutableJSArray
    , create
    , length
    , lengthIO
    , null
    , fromList
    , fromListIO
    , toList
    , toListIO
    , index, (!)
    , read
    , write
    , append
    , push
    , pop
    , unshift
    , shift
    , reverse
    , take
    , takeIO
    , drop
    , dropIO
    , slice
    , sliceIO
    , freeze
    , unsafeFreeze
    , thaw
    , unsafeThaw
    ) where

import Prelude hiding (length, drop, read, take, reverse, null)
import Control.Monad (void)
import Language.Javascript.JSaddle.Types
       (JSM, JSVal, SomeJSArray(..), JSArray,
        MutableJSArray, GHCJSPure(..))
import Control.Lens.Operators ((^.))
import Language.Javascript.JSaddle.Object (js2, js0, (<##), js1, js)
import Language.Javascript.JSaddle.Value (valToNumber)
import JavaScript.Array.Internal (create, fromList, fromListIO, toList, toListIO, index, read, push)

length :: SomeJSArray m -> GHCJSPure Int
length = GHCJSPure . lengthIO
{-# INLINE length #-}

lengthIO :: SomeJSArray m -> JSM Int
lengthIO (SomeJSArray x) = round <$> (x ^. js "length" >>= valToNumber)
{-# INLINE lengthIO #-}

null :: SomeJSArray m -> GHCJSPure Bool
null = GHCJSPure . fmap (== 0) . lengthIO
{-# INLINE null #-}

append :: SomeJSArray m -> SomeJSArray m -> JSM (SomeJSArray m1)
append (SomeJSArray x) (SomeJSArray y) = SomeJSArray <$> x ^. js1 "concat" y
{-# INLINE append #-}

write :: Int -> JSVal -> MutableJSArray -> JSM ()
write n e (SomeJSArray x) = void $ (x <## n) e
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

take :: Int -> SomeJSArray m -> GHCJSPure (SomeJSArray m1)
take n = GHCJSPure . takeIO n
{-# INLINE take #-}

takeIO :: Int -> SomeJSArray m -> JSM (SomeJSArray m1)
takeIO n (SomeJSArray x) = SomeJSArray <$> x ^. js2 "slice" (0::Int) n
{-# INLINE takeIO #-}

drop :: Int -> SomeJSArray m -> GHCJSPure (SomeJSArray m1)
drop n = GHCJSPure . dropIO n
{-# INLINE drop #-}

dropIO :: Int -> SomeJSArray m -> JSM (SomeJSArray m1)
dropIO n (SomeJSArray x) = SomeJSArray <$> x ^. js1 "slice1" n
{-# INLINE dropIO #-}

slice :: Int -> Int -> JSArray -> GHCJSPure (SomeJSArray m1)
slice s n = GHCJSPure . sliceIO s n
{-# INLINE slice #-}

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

(!) :: JSArray -> Int -> GHCJSPure JSVal
x ! n = GHCJSPure $ read n x
{-# INLINE (!) #-}

