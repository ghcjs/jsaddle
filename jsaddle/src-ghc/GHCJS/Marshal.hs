{-# LANGUAGE ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             OverloadedStrings,
             TupleSections,
             LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GHCJS.Marshal ( FromJSVal(..)
                     , ToJSVal(..)
                     , toJSVal_aeson
                     , toJSVal_pure
                     ) where

import           Control.Monad (join)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import qualified Data.Aeson as AE
import           Data.Int (Int8, Int16, Int32)
import           Data.Text (Text)
import           Data.Word (Word8, Word16, Word32, Word)

import           GHC.Prim

import           Language.Javascript.JSaddle.Types (JSM, JSVal, SomeJSArray(..), ghcjsPure)
import           Language.Javascript.JSaddle.Native.Internal
                 (valueToJSONValue, jsonValueToValue, valueToNumber)

import           GHCJS.Types (JSString, isUndefined, isNull)
import           GHCJS.Foreign.Internal (isTruthy)
import           GHCJS.Marshal.Pure ()

import           JavaScript.Array (fromListIO)
import qualified JavaScript.Array as A (read)

import           GHCJS.Marshal.Internal

instance FromJSVal JSVal where
  fromJSValUnchecked x = return x
  {-# INLINE fromJSValUnchecked #-}
  fromJSVal = return . Just
  {-# INLINE fromJSVal #-}
instance FromJSVal () where
  fromJSValUnchecked = fromJSValUnchecked_pure
  {-# INLINE fromJSValUnchecked #-}
  fromJSVal = fromJSVal_pure
--    {-# INLINE fromJSVal #-}
instance FromJSVal Bool where
    fromJSValUnchecked = ghcjsPure . isTruthy
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap Just . ghcjsPure . isTruthy
    {-# INLINE fromJSVal #-}
instance FromJSVal Int where
    fromJSValUnchecked = fmap round . valueToNumber
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap (Just . round) . valueToNumber
    {-# INLINE fromJSVal #-}
instance FromJSVal Int8 where
    fromJSValUnchecked = fmap round . valueToNumber
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap (Just . round) . valueToNumber
    {-# INLINE fromJSVal #-}
instance FromJSVal Int16 where
    fromJSValUnchecked = fmap round . valueToNumber
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap (Just . round) . valueToNumber
    {-# INLINE fromJSVal #-}
instance FromJSVal Int32 where
    fromJSValUnchecked = fmap round . valueToNumber
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap (Just . round) . valueToNumber
    {-# INLINE fromJSVal #-}
instance FromJSVal Word where
    fromJSValUnchecked = fmap round . valueToNumber
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap (Just . round) . valueToNumber
    {-# INLINE fromJSVal #-}
instance FromJSVal Word8 where
    fromJSValUnchecked = fmap round . valueToNumber
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap (Just . round) . valueToNumber
    {-# INLINE fromJSVal #-}
instance FromJSVal Word16 where
    fromJSValUnchecked = fmap round . valueToNumber
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap (Just . round) . valueToNumber
    {-# INLINE fromJSVal #-}
instance FromJSVal Word32 where
    fromJSValUnchecked = fmap round . valueToNumber
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap (Just . round) . valueToNumber
    {-# INLINE fromJSVal #-}
instance FromJSVal Float where
    fromJSValUnchecked = fmap realToFrac . valueToNumber
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap (Just . realToFrac) . valueToNumber
    {-# INLINE fromJSVal #-}
instance FromJSVal Double where
    fromJSValUnchecked = valueToNumber
    {-# INLINE fromJSValUnchecked #-}
    fromJSVal = fmap Just . valueToNumber
    {-# INLINE fromJSVal #-}
instance FromJSVal AE.Value where
    fromJSVal r = Just <$> valueToJSONValue r
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b) => FromJSVal (a,b) where
    fromJSVal r = runMaybeT $ (,) <$> jf r 0 <*> jf r 1
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c) => FromJSVal (a,b,c) where
    fromJSVal r = runMaybeT $ (,,) <$> jf r 0 <*> jf r 1 <*> jf r 2
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d) => FromJSVal (a,b,c,d) where
    fromJSVal r = runMaybeT $ (,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e) => FromJSVal (a,b,c,d,e) where
    fromJSVal r = runMaybeT $ (,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e, FromJSVal f) => FromJSVal (a,b,c,d,e,f) where
    fromJSVal r = runMaybeT $ (,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e, FromJSVal f, FromJSVal g) => FromJSVal (a,b,c,d,e,f,g) where
    fromJSVal r = runMaybeT $ (,,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5 <*> jf r 6
    {-# INLINE fromJSVal #-}
instance (FromJSVal a, FromJSVal b, FromJSVal c, FromJSVal d, FromJSVal e, FromJSVal f, FromJSVal g, FromJSVal h) => FromJSVal (a,b,c,d,e,f,g,h) where
    fromJSVal r = runMaybeT $ (,,,,,,,) <$> jf r 0 <*> jf r 1 <*> jf r 2 <*> jf r 3 <*> jf r 4 <*> jf r 5 <*> jf r 6 <*> jf r 7
    {-# INLINE fromJSVal #-}

jf :: FromJSVal a => JSVal -> Int -> MaybeT JSM a
jf r n = MaybeT $ do
  r' <- A.read n (SomeJSArray r)
  ghcjsPure (isUndefined r) >>= \case
    True -> return Nothing
    False -> fromJSVal r'

instance (ToJSVal a, ToJSVal b) => ToJSVal (a,b) where
    toJSVal (a,b) = join $ arr2 <$> toJSVal a <*> toJSVal b
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c) => ToJSVal (a,b,c) where
    toJSVal (a,b,c) = join $ arr3 <$> toJSVal a <*> toJSVal b <*> toJSVal c
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d) => ToJSVal (a,b,c,d) where
    toJSVal (a,b,c,d) = join $ arr4 <$> toJSVal a <*> toJSVal b <*> toJSVal c <*> toJSVal d
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e) => ToJSVal (a,b,c,d,e) where
    toJSVal (a,b,c,d,e) = join $ arr5 <$> toJSVal a <*> toJSVal b <*> toJSVal c <*> toJSVal d <*> toJSVal e
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e, ToJSVal f) => ToJSVal (a,b,c,d,e,f) where
    toJSVal (a,b,c,d,e,f) = join $ arr6 <$> toJSVal a <*> toJSVal b <*> toJSVal c <*> toJSVal d <*> toJSVal e <*> toJSVal f
    {-# INLINE toJSVal #-}
instance (ToJSVal a, ToJSVal b, ToJSVal c, ToJSVal d, ToJSVal e, ToJSVal f, ToJSVal g) => ToJSVal (a,b,c,d,e,f,g) where
    toJSVal (a,b,c,d,e,f,g) = join $ arr7 <$> toJSVal a <*> toJSVal b <*> toJSVal c <*> toJSVal d <*> toJSVal e <*> toJSVal f <*> toJSVal g
    {-# INLINE toJSVal #-}

arr2 :: JSVal -> JSVal -> JSM JSVal
arr2 a b           = coerce <$> fromListIO [a,b]
arr3 :: JSVal -> JSVal -> JSVal -> JSM JSVal
arr3 a b c         = coerce <$> fromListIO [a,b,c]
arr4 :: JSVal -> JSVal -> JSVal -> JSVal -> JSM JSVal
arr4 a b c d       = coerce <$> fromListIO [a,b,c,d]
arr5 :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSM JSVal
arr5 a b c d e     = coerce <$> fromListIO [a,b,c,d,e]
arr6 :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSM JSVal
arr6 a b c d e f   = coerce <$> fromListIO [a,b,c,d,e,f]
arr7 :: JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSVal -> JSM JSVal
arr7 a b c d e f g = coerce <$> fromListIO [a,b,c,d,e,f,g]

toJSVal_aeson :: AE.ToJSON a => a -> JSM JSVal
toJSVal_aeson = jsonValueToValue . AE.toJSON

