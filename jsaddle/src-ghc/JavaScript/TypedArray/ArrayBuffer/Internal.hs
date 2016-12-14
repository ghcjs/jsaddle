{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module JavaScript.TypedArray.ArrayBuffer.Internal where

import GHCJS.Types

import GHCJS.Internal.Types
import GHCJS.Marshal.Pure

import GHC.Exts (State#)

import Data.Typeable

newtype SomeArrayBuffer (a :: MutabilityType s) =
  SomeArrayBuffer JSVal deriving Typeable
instance IsJSVal (SomeArrayBuffer m)

type ArrayBuffer           = SomeArrayBuffer Immutable
type MutableArrayBuffer    = SomeArrayBuffer Mutable
type STArrayBuffer s       = SomeArrayBuffer (STMutable s)

instance PToJSVal MutableArrayBuffer where
  pToJSVal (SomeArrayBuffer b) = b
instance PFromJSVal MutableArrayBuffer where
  pFromJSVal = SomeArrayBuffer
