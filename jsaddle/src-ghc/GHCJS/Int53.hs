-----------------------------------------------------------------------------
--
-- Module      :  GHCJS.Int53 (native GHC; ghcjs-base has the JavaScript backend version)
-- License     :  MIT
--
-- | Integers that a JavaScript Number holds exactly.
--
-- A JavaScript Number is a double, so it represents every integer of
-- magnitude up to @2^53 - 1@ (@Number.MAX_SAFE_INTEGER@) exactly and loses
-- precision beyond that.  Web IDL's @long long@ and @unsigned long long@
-- are such Numbers (its editors have proposed renaming them @int53@ /
-- @uint53@, <https://github.com/whatwg/webidl/issues/33>), and without
-- @[EnforceRange]@ a browser silently reduces an out of range argument
-- modulo @2^64@.
--
-- 'Int53' and 'Word53' make that range part of the type.  They have no
-- 'Num' instance on purpose: do arithmetic in 'Int64', 'Integer', 'Double'
-- or any other numeric type and convert back with a checked conversion,
-- so the place where a value could go out of range is explicit.
--
-- Using them with the DOM bindings (jsaddle-dom / ghcjs-dom 0.10, where
-- every @long long@ is an 'Int53' and every @unsigned long long@ a
-- 'Word53'):
--
-- > size <- Blob.getSize blob                  -- Word53
-- > let half = fromWord53 size `div` (2 :: Integer)
-- > -- (toInt53 gives Maybe Int53: Nothing when out of range)
-- > part <- Blob.slice blob (Just (int53 0)) (toInt53 half) (Nothing :: Maybe Text)
--
-- Small literals go through 'int53' / 'word53' (every 'Int32' / 'Word32'
-- fits); computed values through 'toInt53' / 'toWord53' (or 'clampInt53'
-- / 'clampWord53' to saturate instead).
--
-- Values read from JavaScript are always in range in practice (a Number
-- holding an integer is within the safe range or is not exactly an
-- integer to begin with).
--
-----------------------------------------------------------------------------
module GHCJS.Int53 (
  -- * Signed
    Int53
  , int53
  , toInt53
  , clampInt53
  , fromInt53
  , minInt53
  , maxInt53
  -- * Unsigned
  , Word53
  , word53
  , toWord53
  , clampWord53
  , fromWord53
  , maxWord53
) where

import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)

import Language.Javascript.JSaddle.Classes (ToJSVal(..), FromJSVal(..))
-- instances for Double
import GHCJS.Marshal ()
import Language.Javascript.JSaddle.Value ()

-- | An integer in the range @[-(2^53 - 1), 2^53 - 1]@.
newtype Int53 = Int53 Int64 deriving (Eq, Ord)

-- | An integer in the range @[0, 2^53 - 1]@.
newtype Word53 = Word53 Word64 deriving (Eq, Ord)

-- | @2^53 - 1@ (@Number.MAX_SAFE_INTEGER@)
safeMax :: Integer
safeMax = 2 ^ (53 :: Int) - 1

minInt53, maxInt53 :: Int53
minInt53 = Int53 (fromInteger (negate safeMax))
maxInt53 = Int53 (fromInteger safeMax)

maxWord53 :: Word53
maxWord53 = Word53 (fromInteger safeMax)

instance Bounded Int53 where
    minBound = minInt53
    maxBound = maxInt53

instance Bounded Word53 where
    minBound = Word53 0
    maxBound = maxWord53

-- | Shown (and read) as a plain number.
instance Show Int53 where
    showsPrec p (Int53 i) = showsPrec p i

instance Show Word53 where
    showsPrec p (Word53 w) = showsPrec p w

instance Read Int53 where
    readsPrec p s = [ (x, rest) | (i, rest) <- readsPrec p s, Just x <- [toInt53 (i :: Integer)] ]

instance Read Word53 where
    readsPrec p s = [ (x, rest) | (i, rest) <- readsPrec p s, Just x <- [toWord53 (i :: Integer)] ]

-- | Every 'Int32' fits (use it for literals: @int53 0@).
int53 :: Int32 -> Int53
int53 = Int53 . fromIntegral

-- | Every 'Word32' fits (use it for literals: @word53 0@).
word53 :: Word32 -> Word53
word53 = Word53 . fromIntegral

-- | 'Nothing' when out of range.
toInt53 :: Integral a => a -> Maybe Int53
toInt53 a
    | abs i <= safeMax = Just (Int53 (fromInteger i))
    | otherwise = Nothing
  where i = toInteger a

-- | 'Nothing' when out of range.
toWord53 :: Integral a => a -> Maybe Word53
toWord53 a
    | i >= 0 && i <= safeMax = Just (Word53 (fromInteger i))
    | otherwise = Nothing
  where i = toInteger a

-- | Saturating conversion (like Web IDL's @[Clamp]@, without rounding).
clampInt53 :: Integral a => a -> Int53
clampInt53 a = Int53 (fromInteger (max (negate safeMax) (min safeMax (toInteger a))))

-- | Saturating conversion (like Web IDL's @[Clamp]@, without rounding).
clampWord53 :: Integral a => a -> Word53
clampWord53 a = Word53 (fromInteger (max 0 (min safeMax (toInteger a))))

-- | Always exact for 'Int64', 'Integer' and 'Double' (and for any type
-- wide enough for the value).
fromInt53 :: Num b => Int53 -> b
fromInt53 (Int53 i) = fromIntegral i

-- | Always exact for 'Word64', 'Int64', 'Integer' and 'Double' (and for any
-- type wide enough for the value).
fromWord53 :: Num b => Word53 -> b
fromWord53 (Word53 w) = fromIntegral w

-- | A JavaScript Number (exact).
instance ToJSVal Int53 where
    toJSVal (Int53 i) = toJSVal (fromIntegral i :: Double)
    {-# INLINE toJSVal #-}

-- | A JavaScript Number (exact).
instance ToJSVal Word53 where
    toJSVal (Word53 w) = toJSVal (fromIntegral w :: Double)
    {-# INLINE toJSVal #-}

-- | 'Nothing' unless the value is a Number holding an integer in range.
instance FromJSVal Int53 where
    fromJSVal v = (>>= fromDouble toInt53) <$> fromJSVal v
    fromJSValUnchecked v = fromJSVal v >>= maybe (fail "fromJSValUnchecked: not an Int53") return

-- | 'Nothing' unless the value is a Number holding an integer in range.
instance FromJSVal Word53 where
    fromJSVal v = (>>= fromDouble toWord53) <$> fromJSVal v
    fromJSValUnchecked v = fromJSVal v >>= maybe (fail "fromJSValUnchecked: not a Word53") return

fromDouble :: (Integer -> Maybe a) -> Double -> Maybe a
fromDouble f d
    | isNaN d || isInfinite d = Nothing
    | fromInteger i == d = f i
    | otherwise = Nothing
  where i = truncate d
