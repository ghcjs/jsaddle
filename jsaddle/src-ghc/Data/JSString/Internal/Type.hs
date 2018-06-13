{-# LANGUAGE DeriveDataTypeable, UnboxedTuples, MagicHash,
             BangPatterns, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}
module Data.JSString.Internal.Type ( JSString(..)
                                   , empty
                                   , empty_
                                   , safe
                                   , firstf
                                   ) where

                                     {-
    -- * Construction
    , text
    , textP
    -- * Safety
    , safe
    -- * Code that must be here for accessibility
    , empty
    , empty_
    -- * Utilities
    , firstf
    -- * Checked multiplication
    , mul
    , mul32
    , mul64
    -- * Debugging
    , showText

                                   ) where
-}
import Control.DeepSeq

import Data.Coerce                    (coerce)
import Data.Text                      (Text)
import qualified Data.Text as T       (empty)
import Data.String                    (IsString)
import Data.Aeson                     (ToJSON(..), FromJSON(..))
import Data.Data                      (Data)
-- import Data.Text.Internal.Unsafe.Char (ord)
import Data.Typeable                  (Typeable)
import Data.Semigroup                 (Semigroup)
import GHC.Exts                       (Char(..), ord#, andI#, (/=#), isTrue#)

-- | A wrapper around a JavaScript string
newtype JSString = JSString Text deriving(Show, Read, IsString, Semigroup, Monoid, Ord, Eq, Data, ToJSON, FromJSON, Typeable)

instance NFData JSString where rnf !_ = ()

-- | /O(1)/ The empty 'JSString'.
empty :: JSString
empty = coerce T.empty
{-# INLINE [1] empty #-}

-- | A non-inlined version of 'empty'.
empty_ :: JSString
empty_ = coerce T.empty
{-# NOINLINE empty_ #-}

safe :: Char -> Char
safe c@(C# cc)
    | isTrue# (andI# (ord# cc) 0x1ff800# /=# 0xd800#) = c
    | otherwise                    = '\xfffd'
{-# INLINE [0] safe #-}


-- | Apply a function to the first element of an optional pair.
firstf :: (a -> c) -> Maybe (a,b) -> Maybe (c,b)
firstf f (Just (a, b)) = Just (f a, b)
firstf _  Nothing      = Nothing

{-
-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul :: Int -> Int -> Int
#if WORD_SIZE_IN_BITS == 64
mul a b = fromIntegral $ fromIntegral a `mul64` fromIntegral b
#else
mul a b = fromIntegral $ fromIntegral a `mul32` fromIntegral b
#endif
{-# INLINE mul #-}
infixl 7 `mul`

-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul64 :: Int64 -> Int64 -> Int64
mul64 a b
  | a >= 0 && b >= 0 =  mul64_ a b
  | a >= 0           = -mul64_ a (-b)
  | b >= 0           = -mul64_ (-a) b
  | otherwise        =  mul64_ (-a) (-b)
{-# INLINE mul64 #-}
infixl 7 `mul64`

mul64_ :: Int64 -> Int64 -> Int64
mul64_ a b
  | ahi > 0 && bhi > 0 = error "overflow"
  | top > 0x7fffffff   = error "overflow"
  | total < 0          = error "overflow"
  | otherwise          = total
  where (# ahi, alo #) = (# a `shiftR` 32, a .&. 0xffffffff #)
        (# bhi, blo #) = (# b `shiftR` 32, b .&. 0xffffffff #)
        top            = ahi * blo + alo * bhi
        total          = (top `shiftL` 32) + alo * blo
{-# INLINE mul64_ #-}

-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul32 :: Int32 -> Int32 -> Int32
mul32 a b = case fromIntegral a * fromIntegral b of
              ab | ab < min32 || ab > max32 -> error "overflow"
                 | otherwise                -> fromIntegral ab
  where min32 = -0x80000000 :: Int64
        max32 =  0x7fffffff
{-# INLINE mul32 #-}
infixl 7 `mul32`
-}
