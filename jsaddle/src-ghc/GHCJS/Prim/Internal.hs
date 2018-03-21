{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}

module GHCJS.Prim.Internal {-( JSVal(..)
                           , JSValueRef
                           , JSException(..)
                           , WouldBlockException(..)
                           , mkJSException
                           , jsNull
                           )-} where

import           Control.DeepSeq (NFData(..))
import           Data.Int (Int64)
import           Data.Typeable (Typeable)
import           Unsafe.Coerce (unsafeCoerce)

import           Data.Aeson (ToJSON(..), FromJSON(..))

import qualified GHC.Exception as Ex
import Data.IORef (newIORef, IORef)
import System.IO.Unsafe (unsafePerformIO)
import Data.Text (Text)
import qualified Data.Aeson as A
import Data.Scientific (toRealFloat)
import Data.Foldable

-- A reference to a particular JavaScript value inside the JavaScript context
type JSValueRef = Int64

{-
  JSVal is a boxed type that can be used as FFI
  argument or result.
-}
newtype JSVal = JSVal { unJSVal :: LazyVal }

data LazyVal = LazyVal
  { _lazyVal_ref :: !(IORef (Maybe Ref)) --TODO: Add a JS-side memory test
  , _lazyVal_val :: Val --TODO: represent at the type level that the Ref (if any) in this Val never refers to a primitive value - so, e.g. it is always truthy
  }

type Val = PrimVal Ref
type ValId = PrimVal RefId

newtype Ref = Ref { unRef :: IORef RefId } deriving (NFData)

-- | Contains all the information that isn't mutable about a javascript object
data PrimVal a
   = PrimVal_Undefined
   | PrimVal_Null
   | PrimVal_Bool Bool
   | PrimVal_Number Double --TODO: Infinities, NaN(s?), negative 0, others?
   | PrimVal_String Text --TODO: Manipulate large strings by reference?
   | PrimVal_Ref a --TODO: Should we rename this to make it clear that it's always a non-null object? Well, it could be a Symbol, or some other implementation-specific thing
   deriving (Functor, Foldable, Traversable, Show, Read, Eq, Ord)

instance ToJSON a => ToJSON (PrimVal a) where
  toJSON = \case
    PrimVal_Undefined -> toJSON ()
    PrimVal_Null -> A.Null
    PrimVal_Bool b -> toJSON b
    PrimVal_Number n -> toJSON n --TODO: NaN
    PrimVal_String s -> toJSON s --TODO: Long strings?
    PrimVal_Ref a -> toJSON [a]

instance FromJSON a => FromJSON (PrimVal a) where
  parseJSON = \case
    A.Null -> return PrimVal_Null
    A.Bool b -> return $ PrimVal_Bool b
    A.Number n -> return $ PrimVal_Number $ toRealFloat n -- TODO: Should we also use Scientific for PrimVal?
    A.String s -> return $ PrimVal_String s
    A.Array a -> case toList a of
      [] -> return PrimVal_Undefined
      [r] -> PrimVal_Ref <$> parseJSON r
      _ -> fail "unexpected array with size > 1"
    A.Object _ -> fail "unexpected object"

-- | A reference to a value that exists in the javascript heap.  If positive, allocated by the Haskell side; if negative, allocated by the javascript side; if zero, always refers to 'undefined'.  In either case, must be freed by the Haskell side using a finalizer.
newtype RefId = RefId { unRefId :: Int64 } deriving (Show, Read, Eq, Ord, Enum, ToJSON, FromJSON)

isJsAllocatedRefId :: RefId -> Bool
isJsAllocatedRefId = (< 0) . unRefId

{-# NOINLINE nothingRef #-}
nothingRef :: IORef (Maybe Ref)
nothingRef = unsafePerformIO $ newIORef Nothing

lazyValFromStrict :: Val -> LazyVal
lazyValFromStrict val = LazyVal
  { _lazyVal_ref = nothingRef
  , _lazyVal_val = val
  }

primToJSVal :: Val -> JSVal
primToJSVal = JSVal . lazyValFromStrict

getLazyVal :: LazyVal -> Val
getLazyVal = _lazyVal_val

getPrimJSVal :: JSVal -> Val
getPrimJSVal = getLazyVal . unJSVal

instance NFData LazyVal where
  rnf (LazyVal a b) = a `seq` b `seq` ()

instance NFData a => NFData (PrimVal a) where
  rnf = \case
    PrimVal_Undefined -> ()
    PrimVal_Null -> ()
    PrimVal_Bool b -> b `seq` ()
    PrimVal_Number n -> n `seq` ()
    PrimVal_String s -> s `seq` ()
    PrimVal_Ref r -> r `seq` ()

{-
  When a JavaScript exception is raised inside
  a safe or interruptible foreign call, it is converted
  to a JSException
 -}
data JSException = JSException JSVal String
  deriving (Typeable)

instance Ex.Exception JSException

instance Show JSException where
  show (JSException _ xs) = "JavaScript exception: " ++ xs

mkJSException :: JSVal -> IO JSException
mkJSException ref =
  return (JSException (unsafeCoerce ref) "")

jsNull :: JSVal
jsNull = primToJSVal PrimVal_Null
{-# NOINLINE jsNull #-}

{- | If a synchronous thread tries to do something that can only
     be done asynchronously, and the thread is set up to not
     continue asynchronously, it receives this exception.
 -}
data WouldBlockException = WouldBlockException
  deriving (Typeable)

instance Show WouldBlockException where
  show _ = "thread would block"

instance Ex.Exception WouldBlockException

