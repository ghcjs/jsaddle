{-# LANGUAGE CPP, ScopedTypeVariables, DeriveDataTypeable, DefaultSignatures,
             TypeOperators, TupleSections, FlexibleContexts, FlexibleInstances,
             LambdaCase
  #-}

module GHCJS.Marshal.Internal ( FromJSVal(..)
                              , ToJSVal(..)
                              , PToJSVal(..)
                              , PFromJSVal(..)
                              , Purity(..)
                              , toJSVal_generic
                              , fromJSVal_generic
                              , toJSVal_pure
                              , fromJSVal_pure
                              , fromJSValUnchecked_pure
                              ) where

import           Control.Applicative
import           Control.Monad

import           Data.Data
import           Data.Maybe
import           Data.Coerce (coerce)
import qualified Data.Text as T (pack)

import           GHC.Generics

import qualified GHCJS.Prim.Internal        as Prim
import qualified GHCJS.Foreign.Internal     as F
import           GHCJS.Types

import qualified Data.JSString.Internal.Type as JSS

import qualified JavaScript.Object.Internal as OI (Object(..), create, setProp, getProp)
import qualified JavaScript.Array.Internal as AI (SomeJSArray(..), create, push, read, fromListIO, toListIO)

import           Language.Javascript.JSaddle.Types (JSM, MutableJSArray, GHCJSPure(..), ghcjsPure, ghcjsPureMap, JSadddleHasCallStack)
import           Language.Javascript.JSaddle.String (textToStr)

data Purity = PureShared    -- ^ conversion is pure even if the original value is shared
            | PureExclusive -- ^ conversion is pure if the we only convert once
  deriving (Eq, Ord, Typeable, Data)

class PToJSVal a where
--  type PureOut a :: Purity
  pToJSVal :: a -> JSVal

class PFromJSVal a where
--  type PureIn a :: Purity
  pFromJSVal :: JSVal -> a

class ToJSVal a where
  toJSVal :: a -> JSM JSVal

  toJSValListOf :: [a] -> JSM JSVal
  toJSValListOf = fmap coerce . AI.fromListIO <=< mapM toJSVal

  -- default toJSVal :: PToJSVal a => a -> JSM (JSVal a)
  -- toJSVal x = return (pToJSVal x)

  default toJSVal :: (Generic a, GToJSVal (Rep a ())) => a -> JSM JSVal
  toJSVal = toJSVal_generic id

instance ToJSVal (SomeJSArray Immutable) where
  toJSVal (SomeJSArray x) = pure x

fromJustWithStack :: JSadddleHasCallStack => Maybe a -> a
fromJustWithStack Nothing = error "fromJSValUnchecked: fromJSVal result was Nothing"
fromJustWithStack (Just x) = x

class FromJSVal a where
  fromJSVal :: JSVal -> JSM (Maybe a)

instance FromJSVal Function where
  fromJSVal = pure . pure . Function . Object

instance FromJSVal Object where
  fromJSVal = pure . pure . Object

#if MIN_VERSION_base(4,9,0) && defined(JSADDLE_HAS_CALL_STACK)
  fromJSValUnchecked :: JSadddleHasCallStack => JSVal -> JSM a
#ifdef CHECK_UNCHECKED
  fromJSValUnchecked v = fromJSVal v >>= \case
                             Nothing -> error "fromJSValUnchecked: fromJSVal result was Nothing"
                             Just x  -> return x
#else
  fromJSValUnchecked = fmap fromJustWithStack . fromJSVal
#endif
#else
  fromJSValUnchecked :: JSVal -> JSM a
  fromJSValUnchecked = fmap fromJust . fromJSVal
#endif
  {-# INLINE fromJSValUnchecked #-}

  fromJSValListOf :: JSVal -> JSM (Maybe [a])
  fromJSValListOf = fmap sequence . (mapM fromJSVal <=< AI.toListIO . coerce) -- fixme should check that it's an array

#if MIN_VERSION_base(4,9,0) && defined(JSADDLE_HAS_CALL_STACK)
  fromJSValUncheckedListOf :: JSadddleHasCallStack => JSVal -> JSM [a]
#else
  fromJSValUncheckedListOf :: JSVal -> JSM [a]
#endif
  fromJSValUncheckedListOf = mapM fromJSValUnchecked <=< AI.toListIO . coerce

  -- default fromJSVal :: PFromJSVal a => JSVal a -> JSM (Maybe a)
  -- fromJSVal x = return (Just (pFromJSVal x))

  default fromJSVal :: (Generic a, GFromJSVal (Rep a ())) => JSVal -> JSM (Maybe a)
  fromJSVal = fromJSVal_generic id

  -- default fromJSValUnchecked :: PFromJSVal a => a -> IO a
  -- fromJSValUnchecked x = return (pFromJSVal x)

-- -----------------------------------------------------------------------------

class GToJSVal a where
  gToJSVal :: (String -> String) -> Bool -> a -> JSM JSVal

class GToJSProp a where
  gToJSProp :: (String -> String) -> JSVal -> a -> JSM ()

class GToJSArr a where
  gToJSArr :: (String -> String) -> MutableJSArray -> a -> JSM ()

instance (ToJSVal b) => GToJSVal (K1 a b c) where
  gToJSVal _ _ (K1 x) = toJSVal x

instance GToJSVal p => GToJSVal (Par1 p) where
  gToJSVal f b (Par1 p) = gToJSVal f b p

instance GToJSVal (f p) => GToJSVal (Rec1 f p) where
  gToJSVal f b (Rec1 x) = gToJSVal f b x

instance (GToJSVal (a p), GToJSVal (b p)) => GToJSVal ((a :+: b) p) where
  gToJSVal f _ (L1 x) = gToJSVal f True x
  gToJSVal f _ (R1 x) = gToJSVal f True x

instance (Datatype c, GToJSVal (a p)) => GToJSVal (M1 D c a p) where
  gToJSVal f b m@(M1 x) = gToJSVal f b x

instance (Constructor c, GToJSVal (a p)) => GToJSVal (M1 C c a p) where
  gToJSVal f True m@(M1 x) = do
    obj@(OI.Object obj') <- OI.create
    v   <- gToJSVal f (conIsRecord m) x
    OI.setProp (packJSS . f $ conName m) v obj
    return obj'
  gToJSVal f _ m@(M1 x) = gToJSVal f (conIsRecord m) x

instance (GToJSArr (a p), GToJSArr (b p), GToJSProp (a p), GToJSProp (b p)) => GToJSVal ((a :*: b) p) where
  gToJSVal f True xy = do
    (OI.Object obj') <- OI.create
    gToJSProp f obj' xy
    return obj'
  gToJSVal f False xy = do
    arr@(AI.SomeJSArray arr') <- AI.create
    gToJSArr f arr xy
    return arr'

instance GToJSVal (a p) => GToJSVal (M1 S c a p) where
  gToJSVal f b (M1 x) = gToJSVal f b x

instance (GToJSProp (a p), GToJSProp (b p)) => GToJSProp ((a :*: b) p) where
  gToJSProp f o (x :*: y) = gToJSProp f o x >> gToJSProp f o y

instance (Selector c, GToJSVal (a p)) => GToJSProp (M1 S c a p) where
  gToJSProp f o m@(M1 x) = do
    r <- gToJSVal f False x
    OI.setProp (packJSS . f $ selName m) r (OI.Object o)

instance (GToJSArr (a p), GToJSArr (b p)) => GToJSArr ((a :*: b) p) where
  gToJSArr f a (x :*: y) = gToJSArr f a x >> gToJSArr f a y

instance GToJSVal (a p) => GToJSArr (M1 S c a p) where
  gToJSArr f a (M1 x) = do
    r <- gToJSVal f False x
    AI.push r a

instance GToJSVal (V1 p) where
  gToJSVal _ _ _ = return Prim.jsNull

instance GToJSVal (U1 p) where
  gToJSVal _ _ _ = return F.jsTrue

toJSVal_generic :: forall a . (Generic a, GToJSVal (Rep a ()))
                => (String -> String) -> a -> JSM JSVal
toJSVal_generic f x = gToJSVal f False (from x :: Rep a ())

-- -----------------------------------------------------------------------------

class GFromJSVal a where
  gFromJSVal :: (String -> String) -> Bool -> JSVal -> JSM (Maybe a)

class GFromJSProp a where
  gFromJSProp :: (String -> String) -> JSVal -> JSM (Maybe a)

class GFromJSArr a where
  gFromJSArr :: (String -> String) -> MutableJSArray -> Int -> JSM (Maybe (a,Int))

instance FromJSVal b => GFromJSVal (K1 a b c) where
  gFromJSVal _ _ r = fmap K1 <$> fromJSVal r

instance GFromJSVal p => GFromJSVal (Par1 p) where
  gFromJSVal f b r = gFromJSVal f b r

instance GFromJSVal (f p) => GFromJSVal (Rec1 f p) where
  gFromJSVal f b r = gFromJSVal f b r

instance (GFromJSVal (a p), GFromJSVal (b p)) => GFromJSVal ((a :+: b) p) where
  gFromJSVal f b r = do
    l <- gFromJSVal f True r
    case l of
      Just x  -> return (L1 <$> Just x)
      Nothing -> fmap R1 <$> gFromJSVal f True r

instance (Datatype c, GFromJSVal (a p)) => GFromJSVal (M1 D c a p) where
  gFromJSVal f b r = fmap M1 <$> gFromJSVal f b r

instance forall c a p . (Constructor c, GFromJSVal (a p)) => GFromJSVal (M1 C c a p) where
  gFromJSVal f True r = do
    r' <- OI.getProp (packJSS . f $ conName (undefined :: M1 C c a p)) (OI.Object r)
    ghcjsPure (isUndefined r') >>= \case
      True -> return Nothing
      False -> fmap M1 <$> gFromJSVal f (conIsRecord (undefined :: M1 C c a p)) r'
  gFromJSVal f _ r = fmap M1 <$> gFromJSVal f (conIsRecord (undefined :: M1 C c a p)) r

instance (GFromJSArr (a p), GFromJSArr (b p), GFromJSProp (a p), GFromJSProp (b p)) => GFromJSVal ((a :*: b) p) where
  gFromJSVal f True  r = gFromJSProp f r
  gFromJSVal f False r = fmap fst <$> gFromJSArr f (AI.SomeJSArray r) 0

instance GFromJSVal (a p) => GFromJSVal (M1 S c a p) where
  gFromJSVal f b r = fmap M1 <$> gFromJSVal f b r

instance (GFromJSProp (a p), GFromJSProp (b p)) => GFromJSProp ((a :*: b) p) where
  gFromJSProp f r = do
    a <- gFromJSProp f r
    case a of
      Nothing -> return Nothing
      Just a' -> fmap (a':*:) <$> gFromJSProp f r

instance forall c a p . (Selector c, GFromJSVal (a p)) => GFromJSProp (M1 S c a p) where
  gFromJSProp f o = do
    p <- OI.getProp (packJSS . f $ selName (undefined :: M1 S c a p)) (OI.Object o)
    ghcjsPure (isUndefined p) >>= \case
      True -> return Nothing
      False -> fmap M1 <$> gFromJSVal f False p

instance (GFromJSArr (a p), GFromJSArr (b p)) => GFromJSArr ((a :*: b) p) where
  gFromJSArr f r _n = do
    a <- gFromJSArr f r 0
    case a of
      Just (a',an) -> do
        b <- gFromJSArr f r an
        case b of
          Just (b',bn) -> return (Just (a' :*: b',bn))
          _            -> return Nothing

instance (GFromJSVal (a p)) => GFromJSArr (M1 S c a p) where
  gFromJSArr f o n = do
    r <- AI.read n o
    ghcjsPure (isUndefined r) >>= \case
      True -> return Nothing
      False -> fmap ((,n+1) . M1) <$> gFromJSVal f False r

instance GFromJSVal (V1 p) where
  gFromJSVal _ _ _ = return Nothing

instance GFromJSVal (U1 p) where
  gFromJSVal _ _ _ = return (Just U1)

fromJSVal_generic :: forall a . (Generic a, GFromJSVal (Rep a ()))
                => (String -> String) -> JSVal -> JSM (Maybe a)
fromJSVal_generic f x = fmap to <$> (gFromJSVal f False x :: JSM (Maybe (Rep a ())))

-- -----------------------------------------------------------------------------

fromJSVal_pure :: PFromJSVal a => JSVal -> JSM (Maybe a)
fromJSVal_pure = return . Just . pFromJSVal
{-# INLINE fromJSVal_pure #-}

fromJSValUnchecked_pure :: PFromJSVal a => JSVal -> JSM a
fromJSValUnchecked_pure = return . pFromJSVal
{-# INLINE fromJSValUnchecked_pure #-}

toJSVal_pure :: PToJSVal a => a -> JSM JSVal
toJSVal_pure = return . pToJSVal
{-# INLINE toJSVal_pure #-}

-- -----------------------------------------------------------------------------

packJSS :: String -> JSString
packJSS = textToStr . T.pack
