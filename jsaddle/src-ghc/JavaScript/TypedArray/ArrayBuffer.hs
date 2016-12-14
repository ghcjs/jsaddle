module JavaScript.TypedArray.ArrayBuffer
    ( ArrayBuffer
    , MutableArrayBuffer
    , freeze, unsafeFreeze
    , thaw, unsafeThaw
    , byteLengthIO
    ) where

import Control.Lens.Operators ((^.))

import GHCJS.Marshal (fromJSValUnchecked)

import Language.Javascript.JSaddle.Types (JSM)
import Language.Javascript.JSaddle.Object (jsg, new, js, js1, js2)
import JavaScript.TypedArray.ArrayBuffer.Internal

create :: Int -> JSM MutableArrayBuffer
create n = SomeArrayBuffer <$> new (jsg "ArrayBuffer") [n]
{-# INLINE create #-}

{- | Create an immutable 'ArrayBuffer' by copying a 'MutableArrayBuffer' -}
freeze :: MutableArrayBuffer -> JSM ArrayBuffer
freeze (SomeArrayBuffer b) = SomeArrayBuffer <$> b ^. js1 "slice" (0 :: Int)
{-# INLINE freeze #-}

{- | Create an immutable 'ArrayBuffer' from a 'MutableArrayBuffer' without
     copying. The result shares the buffer with the argument,  not modify
     the data in the 'MutableArrayBuffer' after freezing
 -}
unsafeFreeze :: MutableArrayBuffer -> JSM ArrayBuffer
unsafeFreeze (SomeArrayBuffer b) = pure (SomeArrayBuffer b)
{-# INLINE unsafeFreeze #-}

{- | Create a 'MutableArrayBuffer' by copying an immutable 'ArrayBuffer' -}
thaw :: ArrayBuffer -> JSM MutableArrayBuffer
thaw (SomeArrayBuffer b) = SomeArrayBuffer <$> b ^. js1 "slice" (0 :: Int)
{-# INLINE thaw #-}

unsafeThaw :: ArrayBuffer -> JSM MutableArrayBuffer
unsafeThaw (SomeArrayBuffer b) = pure (SomeArrayBuffer b)
{-# INLINE unsafeThaw #-}

sliceIO :: Int -> Maybe Int -> SomeArrayBuffer any -> JSM (SomeArrayBuffer any)
sliceIO begin (Just end) (SomeArrayBuffer b) = SomeArrayBuffer <$> b ^. js2 "slice" begin end
sliceIO begin _          (SomeArrayBuffer b) = SomeArrayBuffer <$> b ^. js1 "slice" begin
{-# INLINE sliceIO #-}

byteLengthIO :: SomeArrayBuffer any -> JSM Int
byteLengthIO (SomeArrayBuffer b) = b ^. js "byteLength" >>= fromJSValUnchecked
{-# INLINE byteLengthIO #-}

