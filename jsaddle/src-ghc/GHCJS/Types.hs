{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module GHCJS.Types ( JSVal
                   , WouldBlockException(..)
                   , JSException(..)
                   , IsJSVal
                   , jsval
                   , isNull
                   , isUndefined
                   , nullRef
                   , JSString
                   , mkRef
                   , Ref#
--                   , toPtr
--                   , fromPtr
                   , JSRef
                   ) where

import Data.JSString.Internal.Type (JSString)
import GHCJS.Internal.Types

import GHCJS.Prim

import GHC.Int
import GHC.Types
import GHC.Prim
import GHC.Ptr
import GHC.IORef
import GHC.IO.Unsafe (unsafePerformIO)

import Control.DeepSeq
import Unsafe.Coerce

type Ref# = IORef Int64

mkRef :: Ref# -> JSVal
mkRef = JSVal
{-# INLINE mkRef #-}

nullRef :: JSVal
nullRef = JSVal . unsafePerformIO $ newIORef 0
{-# NOINLINE nullRef #-}

--toPtr :: JSVal -> Ptr a
--toPtr (JSVal x) = unsafeCoerce (Ptr' x 0#)
--{-# INLINE toPtr #-}
--
--fromPtr :: Ptr a -> JSVal
--fromPtr p = js_ptrVal p
--{-# INLINE fromPtr #-}
--
--data Ptr' a = Ptr' ByteArray# Int#

-- | This is a deprecated copmatibility wrapper for the old JSRef type.
--
-- See https://github.com/ghcjs/ghcjs/issues/421
type JSRef a = JSVal
{-# DEPRECATED JSRef "Use JSVal instead, or a more specific newtype wrapper of JSVal " #-}
