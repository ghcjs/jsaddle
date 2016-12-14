{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module JavaScript.TypedArray.Internal where

import Prelude hiding ((!!))

import GHC.Int
import GHC.Word

import GHCJS.Internal.Types

import Control.Monad (void)
import Control.Lens.Operators ((^.))

import GHCJS.Marshal (fromJSValUnchecked)

import JavaScript.Array.Internal (SomeJSArray(..))
import JavaScript.TypedArray.ArrayBuffer
import JavaScript.TypedArray.ArrayBuffer.Internal (SomeArrayBuffer(..))
import JavaScript.TypedArray.Internal.Types

import Language.Javascript.JSaddle.Types (JSM, GHCJSPure(..))
import Language.Javascript.JSaddle.Object (js, jsg, js1, js2, new, (!!), (<##))

elemSize :: SomeTypedArray e m -> GHCJSPure Int
elemSize (SomeTypedArray a) = GHCJSPure $ a ^. js "BYTES_PER_ELEMENT" >>= fromJSValUnchecked
{-# INLINE [1] elemSize #-}
{-# RULES "elemSizeUint8Clamped" forall (x :: SomeUint8ClampedArray m). elemSize x = GHCJSPure $ return 1 #-}
{-# RULES "elemSizeUint8"        forall (x :: SomeUint8Array m).        elemSize x = GHCJSPure $ return 1 #-}
{-# RULES "elemSizeUint16"       forall (x :: SomeUint16Array m).       elemSize x = GHCJSPure $ return 2 #-}
{-# RULES "elemSizeUint32"       forall (x :: SomeUint32Array m).       elemSize x = GHCJSPure $ return 4 #-}
{-# RULES "elemSizeInt8"         forall (x :: SomeInt8Array m).         elemSize x = GHCJSPure $ return 1 #-}
{-# RULES "elemSizeInt16"        forall (x :: SomeInt16Array m).        elemSize x = GHCJSPure $ return 2 #-}
{-# RULES "elemSizeInt32"        forall (x :: SomeInt32Array m).        elemSize x = GHCJSPure $ return 4 #-}
{-# RULES "elemSizeFloat32"      forall (x :: SomeFloat32Array m).      elemSize x = GHCJSPure $ return 4 #-}
{-# RULES "elemSizeFloat64"      forall (x :: SomeFloat64Array m).      elemSize x = GHCJSPure $ return 8 #-}

instance TypedArray IOInt8Array where
  index              = indexI8
  unsafeIndex        = unsafeIndexI8
  setIndex i x       = setIndexI i (fromIntegral x)
  unsafeSetIndex i x = unsafeSetIndexI i (fromIntegral x)
  indexOf s x        = indexOfI s (fromIntegral x)
  lastIndexOf s x    = lastIndexOfI s (fromIntegral x)
  create l           = SomeTypedArray <$> new (jsg "Int8Array") [l]
  fromArray          = int8ArrayFrom
  fromArrayBuffer    = undefined

instance TypedArray IOInt16Array where
  index              = indexI16
  unsafeIndex        = unsafeIndexI16
  setIndex i x       = setIndexI i (fromIntegral x)
  unsafeSetIndex i x = unsafeSetIndexI i (fromIntegral x)
  indexOf s x        = indexOfI s (fromIntegral x)
  lastIndexOf s x    = lastIndexOfI s (fromIntegral x)
  create l           = SomeTypedArray <$> new (jsg "Int16Array") [l]
  fromArray          = int16ArrayFrom
  fromArrayBuffer    = undefined

instance TypedArray IOInt32Array where
  index           = indexI
  unsafeIndex     = unsafeIndexI
  setIndex        = setIndexI
  unsafeSetIndex  = unsafeSetIndexI
  indexOf         = indexOfI
  lastIndexOf     = lastIndexOfI
  create l        = SomeTypedArray <$> new (jsg "Int32Array") [l]
  fromArray       = int32ArrayFrom
  fromArrayBuffer = undefined

instance TypedArray IOUint8ClampedArray where
  index              = indexW8
  unsafeIndex        = unsafeIndexW8
  setIndex i x       = setIndexW i (fromIntegral x)
  unsafeSetIndex i x = unsafeSetIndexW i (fromIntegral x)
  indexOf s x        = indexOfW s (fromIntegral x)
  lastIndexOf s x    = lastIndexOfW s (fromIntegral x)
  create l           = SomeTypedArray <$> new (jsg "Uint8ClampedArray") [l]
  fromArray          = uint8ClampedArrayFrom
  fromArrayBuffer    = undefined

instance TypedArray IOUint8Array where
  index              = indexW8
  unsafeIndex        = unsafeIndexW8
  setIndex i x       = setIndexW i (fromIntegral x)
  unsafeSetIndex i x = unsafeSetIndexW i (fromIntegral x)
  indexOf s x        = indexOfW s (fromIntegral x)
  lastIndexOf s x    = lastIndexOfW s (fromIntegral x)
  create l           = SomeTypedArray <$> new (jsg "Uint8Array") [l]
  fromArray          = uint8ArrayFrom
  fromArrayBuffer    = undefined

instance TypedArray IOUint16Array where
  index              = indexW16
  unsafeIndex        = unsafeIndexW16
  setIndex i x       = setIndexW i (fromIntegral x)
  unsafeSetIndex i x = unsafeSetIndexW i (fromIntegral x)
  indexOf s x        = indexOfW s (fromIntegral x)
  lastIndexOf s x    = lastIndexOfW s (fromIntegral x)
  create l           = SomeTypedArray <$> new (jsg "Uint16Array") [l]
  fromArray          = uint16ArrayFrom
  fromArrayBuffer    = undefined

instance TypedArray IOUint32Array where
  index           = indexW
  unsafeIndex     = unsafeIndexW
  setIndex        = setIndexW
  unsafeSetIndex  = unsafeSetIndexW
  indexOf         = indexOfW
  lastIndexOf     = lastIndexOfW
  create l        = SomeTypedArray <$> new (jsg "Uint32Array") [l]
  fromArray       = uint32ArrayFrom
  fromArrayBuffer = undefined

instance TypedArray IOFloat32Array where
  index           = indexD
  unsafeIndex     = unsafeIndexD
  setIndex        = setIndexD
  unsafeSetIndex  = unsafeSetIndexD
  indexOf         = indexOfD
  lastIndexOf     = lastIndexOfD
  create l        = SomeTypedArray <$> new (jsg "Float32Array") [l]
  fromArray       = float32ArrayFrom
  fromArrayBuffer = undefined

instance TypedArray IOFloat64Array where
  index           = indexD
  unsafeIndex     = unsafeIndexD
  setIndex        = setIndexD
  unsafeSetIndex  = unsafeSetIndexD
  indexOf         = indexOfD
  lastIndexOf     = lastIndexOfD
  create l        = SomeTypedArray <$> new (jsg "Float64Array") [l]
  fromArray       = float64ArrayFrom
  fromArrayBuffer = undefined


class TypedArray a where
  unsafeIndex     :: Int           -> a -> JSM (Elem a)
  index           :: Int           -> a -> JSM (Elem a)
  unsafeSetIndex  :: Int -> Elem a -> a -> JSM ()
  setIndex        :: Int -> Elem a -> a -> JSM ()
  create          :: Int                -> JSM a
  fromArray       :: SomeJSArray m      -> JSM a
  fromArrayBuffer :: MutableArrayBuffer -> Int    -> Maybe Int -> JSM a
  indexOf         :: Int                -> Elem a -> a -> JSM Int
  lastIndexOf     :: Int                -> Elem a -> a -> JSM Int

-- -----------------------------------------------------------------------------

indexI :: Int -> SomeTypedArray e m -> JSM Int
indexI i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE indexI #-}

indexI16 :: Int -> SomeTypedArray e m -> JSM Int16
indexI16 i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE indexI16 #-}

indexI8 :: Int -> SomeTypedArray e m -> JSM Int8
indexI8 i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE indexI8 #-}

indexW :: Int -> SomeTypedArray e m -> JSM Word
indexW i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE indexW #-}

indexW16 :: Int -> SomeTypedArray e m -> JSM Word16
indexW16 i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE indexW16 #-}

indexW8 :: Int -> SomeTypedArray e m -> JSM Word8
indexW8 i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE indexW8 #-}

indexD :: Int -> SomeTypedArray e m -> JSM Double
indexD i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE indexD #-}

-- -----------------------------------------------------------------------------

unsafeIndexI :: Int -> SomeTypedArray e m -> JSM Int
unsafeIndexI i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE unsafeIndexI #-}

unsafeIndexI16 :: Int -> SomeTypedArray e m -> JSM Int16
unsafeIndexI16 i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE unsafeIndexI16 #-}

unsafeIndexI8 :: Int -> SomeTypedArray e m -> JSM Int8
unsafeIndexI8 i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE unsafeIndexI8 #-}

unsafeIndexW :: Int -> SomeTypedArray e m -> JSM  Word
unsafeIndexW i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE unsafeIndexW #-}

unsafeIndexW16 :: Int -> SomeTypedArray e m -> JSM Word16
unsafeIndexW16 i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE unsafeIndexW16 #-}

unsafeIndexW8 :: Int -> SomeTypedArray e m -> JSM Word8
unsafeIndexW8 i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE unsafeIndexW8 #-}

unsafeIndexD :: Int -> SomeTypedArray e m -> JSM Double
unsafeIndexD i (SomeTypedArray a) = a !! i >>= fromJSValUnchecked
{-# INLINE unsafeIndexD #-}

-- -----------------------------------------------------------------------------

int8ArrayFrom :: SomeJSArray m0 -> JSM (SomeInt8Array m1)
int8ArrayFrom (SomeJSArray a) = SomeTypedArray <$> jsg "Int8Array" ^. js1 "from" a
{-# INLINE int8ArrayFrom #-}

int16ArrayFrom :: SomeJSArray m0 -> JSM (SomeInt16Array m1)
int16ArrayFrom (SomeJSArray a) = SomeTypedArray <$> jsg "Int16Array" ^. js1 "from" a
{-# INLINE int16ArrayFrom #-}

int32ArrayFrom :: SomeJSArray m0 -> JSM (SomeInt32Array m1)
int32ArrayFrom (SomeJSArray a) = SomeTypedArray <$> jsg "Int32Array" ^. js1 "from" a
{-# INLINE int32ArrayFrom #-}

uint8ArrayFrom :: SomeJSArray m0 -> JSM (SomeUint8Array m1)
uint8ArrayFrom (SomeJSArray a) = SomeTypedArray <$> jsg "Uint8Array" ^. js1 "from" a
{-# INLINE uint8ArrayFrom #-}

uint8ClampedArrayFrom :: SomeJSArray m0 -> JSM (SomeUint8ClampedArray m1)
uint8ClampedArrayFrom (SomeJSArray a) = SomeTypedArray <$> jsg "Uint8ClampedArray" ^. js1 "from" a
{-# INLINE uint8ClampedArrayFrom #-}

uint16ArrayFrom :: SomeJSArray m0 -> JSM (SomeUint16Array m1)
uint16ArrayFrom (SomeJSArray a) = SomeTypedArray <$> jsg "Uint16Array" ^. js1 "from" a
{-# INLINE uint16ArrayFrom #-}

uint32ArrayFrom :: SomeJSArray m0 -> JSM (SomeUint32Array m1)
uint32ArrayFrom (SomeJSArray a) = SomeTypedArray <$> jsg "Uint32Array" ^. js1 "from" a
{-# INLINE uint32ArrayFrom #-}

float32ArrayFrom :: SomeJSArray m0 -> JSM (SomeFloat32Array m1)
float32ArrayFrom (SomeJSArray a) = SomeTypedArray <$> jsg "Float32Array" ^. js1 "from" a
{-# INLINE float32ArrayFrom #-}

float64ArrayFrom :: SomeJSArray m0 -> JSM (SomeFloat64Array m1)
float64ArrayFrom (SomeJSArray a) = SomeTypedArray <$> jsg "Float64Array" ^. js1 "from" a
{-# INLINE float64ArrayFrom #-}

-- -----------------------------------------------------------------------------

setIndexI :: Mutability m ~ IsMutable
          => Int -> Int -> SomeTypedArray e m -> JSM ()
setIndexI i x (SomeTypedArray a) = (a <## i) x
{-# INLINE setIndexI #-}

unsafeSetIndexI :: Mutability m ~ IsMutable
                => Int -> Int -> SomeTypedArray e m -> JSM ()
unsafeSetIndexI i x (SomeTypedArray a) = (a <## i) x
{-# INLINE unsafeSetIndexI #-}

setIndexW :: Mutability m ~ IsMutable
           => Int -> Word -> SomeTypedArray e m -> JSM ()
setIndexW i x (SomeTypedArray a) = (a <## i) x
{-# INLINE setIndexW #-}

unsafeSetIndexW :: Mutability m ~ IsMutable
                => Int -> Word -> SomeTypedArray e m -> JSM ()
unsafeSetIndexW i x (SomeTypedArray a) = (a <## i) x
{-# INLINE unsafeSetIndexW #-}

setIndexD :: Mutability m ~ IsMutable
          => Int -> Double -> SomeTypedArray e m -> JSM ()
setIndexD i x (SomeTypedArray a) = (a <## i) x
{-# INLINE setIndexD #-}

unsafeSetIndexD :: Mutability m ~ IsMutable
                => Int -> Double -> SomeTypedArray e m -> JSM ()
unsafeSetIndexD i x (SomeTypedArray a) = (a <## i) x
{-# INLINE unsafeSetIndexD #-}

indexOfI :: Mutability m ~ IsMutable
         => Int -> Int -> SomeTypedArray e m -> JSM Int
indexOfI s x (SomeTypedArray a) = a ^. js2 "indexOf" x s >>= fromJSValUnchecked
{-# INLINE indexOfI #-}

indexOfW :: Int -> Word -> SomeTypedArray e m -> JSM Int
indexOfW s x (SomeTypedArray a) = a ^. js2 "indexOf" x s >>= fromJSValUnchecked
{-# INLINE indexOfW #-}

indexOfD :: Int -> Double -> SomeTypedArray e m -> JSM Int
indexOfD s x (SomeTypedArray a) = a ^. js2 "indexOf" x s >>= fromJSValUnchecked
{-# INLINE indexOfD #-}

lastIndexOfI :: Int -> Int -> SomeTypedArray e m -> JSM Int
lastIndexOfI s x (SomeTypedArray a) = a ^. js2 "lastIndexOf" x s >>= fromJSValUnchecked
{-# INLINE lastIndexOfI #-}

lastIndexOfW :: Int -> Word -> SomeTypedArray e m -> JSM Int
lastIndexOfW s x (SomeTypedArray a) = a ^. js2 "lastIndexOf" x s >>= fromJSValUnchecked
{-# INLINE lastIndexOfW #-}

lastIndexOfD :: Int -> Double -> SomeTypedArray e m -> JSM Int
lastIndexOfD s x (SomeTypedArray a) = a ^. js2 "lastIndexOf" x s >>= fromJSValUnchecked
{-# INLINE lastIndexOfD #-}

-- -----------------------------------------------------------------------------
-- non-class operations usable for all typed array
{-| length of the typed array in elements -}
length :: SomeTypedArray e m -> GHCJSPure Int
length (SomeTypedArray a) = GHCJSPure $ a ^. js "length" >>= fromJSValUnchecked
{-# INLINE length #-}

{-| length of the array in bytes -}
byteLength :: SomeTypedArray e m -> GHCJSPure Int
byteLength (SomeTypedArray a) = GHCJSPure $ a ^. js "byteLength" >>= fromJSValUnchecked
{-# INLINE byteLength #-}

{-| offset of the array in the buffer -}
byteOffset :: SomeTypedArray e m -> GHCJSPure Int
byteOffset (SomeTypedArray a) = GHCJSPure $ a ^. js "byteOffset" >>= fromJSValUnchecked
{-# INLINE byteOffset #-}

{-| the underlying buffer of the array -}
buffer :: SomeTypedArray e m -> GHCJSPure (SomeArrayBuffer m)
buffer (SomeTypedArray a) = GHCJSPure $ SomeArrayBuffer <$> a ^. js "buffer"
{-# INLINE buffer #-}

{-| create a view of the existing array -}
subarray :: Int -> Int -> SomeTypedArray e m -> GHCJSPure (SomeTypedArray e m)
subarray begin end (SomeTypedArray a) = GHCJSPure$ SomeTypedArray <$> a ^. js2 "subarray" begin end
{-# INLINE subarray #-}

-- fixme convert JSException to Haskell exception
{-| copy the elements of one typed array to another -}
set :: Int -> SomeTypedArray e m -> SomeTypedArray e1 Mutable -> GHCJSPure ()
set offset (SomeTypedArray src) (SomeTypedArray dest) = GHCJSPure $ void $ dest ^. js2 "set" offset src
{-# INLINE set #-}

unsafeSet :: Int -> SomeTypedArray e m -> SomeTypedArray e1 Mutable -> GHCJSPure ()
unsafeSet offset (SomeTypedArray src) (SomeTypedArray dest) = GHCJSPure $ void $ dest ^. js2 "set" offset src
{-# INLINE unsafeSet #-}

