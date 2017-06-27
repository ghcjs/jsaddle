{-# LANGUAGE CPP                        #-}
#ifdef ghcjs_HOST_OS
{-# OPTIONS_GHC -Wno-dodgy-exports      #-}
#else
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ImplicitParams             #-}
#endif
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Types
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Types (
  -- * JavaScript Context
    JSContextRef(..)

  -- * The JSM Monad
  , JSM(..)
  , MonadJSM(..)
  , liftJSM

  -- * Pure GHCJS functions
  , GHCJSPure(..)
  , ghcjsPure
  , ghcjsPureMap
  , ghcjsPureId

  -- * JavaScript Value Types
  , JSVal(..)
  , IsJSVal(..)
  , jsval
  , SomeJSArray(..)
  , JSArray
  , MutableJSArray
  , STJSArray
  , Object(..)
  , JSString(..)
  , Nullable(..)
  , JSCallAsFunction

  -- * Debugging
  , JSadddleHasCallStack

  -- * JavaScript Context Commands
#ifndef ghcjs_HOST_OS
  , MutabilityType(..)
  , Mutable
  , Immutable
  , IsItMutable(..)
  , Mutability
  , JSValueReceived(..)
  , JSValueForSend(..)
  , JSStringReceived(..)
  , JSStringForSend(..)
  , JSObjectForSend(..)
  , AsyncCommand(..)
  , Command(..)
  , Batch(..)
  , Result(..)
  , BatchResults(..)
  , Results(..)
#endif
) where

import Control.Monad.IO.Class (MonadIO(..))
#ifdef ghcjs_HOST_OS
import GHCJS.Types
import JavaScript.Object.Internal (Object(..))
import JavaScript.Array.Internal (SomeJSArray(..), JSArray, MutableJSArray, STJSArray)
import GHCJS.Nullable (Nullable(..))
#else
import GHCJS.Prim.Internal (JSVal(..), JSValueRef)
import Data.JSString.Internal.Type (JSString(..))
import Control.DeepSeq (NFData(..))
import Control.Monad.Trans.Cont (ContT(..))
import Control.Monad.Trans.Error (Error(..), ErrorT(..))
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.List (ListT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.RWS.Lazy as Lazy (RWST(..))
import Control.Monad.Trans.RWS.Strict as Strict (RWST(..))
import Control.Monad.Trans.State.Lazy as Lazy (StateT(..))
import Control.Monad.Trans.State.Strict as Strict (StateT(..))
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT(..))
import Control.Monad.Trans.Writer.Strict as Strict (WriterT(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Ref (MonadAtomicRef(..), MonadRef(..))
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.MVar (MVar)
import Data.Int (Int64)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import Data.Typeable (Typeable)
import Data.Coerce (coerce, Coercible)
import Data.Aeson
       (defaultOptions, genericToEncoding, ToJSON(..), FromJSON(..), Value)
import GHC.Generics (Generic)
#endif

#if MIN_VERSION_base(4,9,0) && defined(CHECK_UNCHECKED)
import GHC.Stack (HasCallStack)
#else
import GHC.Exts (Constraint)
#endif

-- | Identifies a JavaScript execution context.
--   When using GHCJS this is just '()' since their is only one context.
--   When using GHC it includes the functions JSaddle needs to communicate
--   with the JavaScript context.
#ifdef ghcjs_HOST_OS
type JSContextRef = ()
#else
data JSContextRef = JSContextRef {
    contextId              :: Int64
  , startTime              :: UTCTime
  , doSendCommand          :: Command -> IO Result
  , doSendAsyncCommand     :: AsyncCommand -> IO ()
  , addCallback            :: Object -> JSCallAsFunction -> IO ()
  , nextRef                :: TVar JSValueRef
  , doEnableLogging        :: Bool -> IO ()
  , finalizerThreads       :: MVar (Set Text)
  , animationFrameHandlers :: MVar [Double -> JSM ()]
}
#endif

-- | The 'JSM' monad keeps track of the JavaScript execution context.
--
--   When using GHCJS it is `IO`.
--
--   Given a 'JSM' function and a 'JSContextRef' you can run the
--   function like this...
--
-- > runJSM jsmFunction javaScriptContext
#ifdef ghcjs_HOST_OS
type JSM = IO
#else
newtype JSM a = JSM { unJSM :: ReaderT JSContextRef IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
#endif


-- | Type we can give to functions that are pure when using ghcjs, but
--   live in JSM when using jsaddle.
--
--   Some functions that can be pure in GHCJS cannot be implemented in
--   a pure way in JSaddle (because we need to know the JSContextRef).
--   Instead we implement versions of these functions in that return
--   `GHCJSPure a` instead of `a`.  To call them in a way that will
--   work when compiling with GHCJS use `ghcjsPure`.
#ifdef ghcjs_HOST_OS
type GHCJSPure a = a
#else
newtype GHCJSPure a = GHCJSPure (JSM a)
#endif

-- | Used when you want to call a functions that is pure in GHCJS, but
--   lives in the JSM in jsaddle.
ghcjsPure :: GHCJSPure a -> JSM a
#ifdef ghcjs_HOST_OS
ghcjsPure = pure
#else
ghcjsPure (GHCJSPure x) = x
#endif
{-# INLINE ghcjsPure #-}

ghcjsPureMap :: (a -> b) -> GHCJSPure a -> GHCJSPure b
#ifdef ghcjs_HOST_OS
ghcjsPureMap = id
#else
ghcjsPureMap f (GHCJSPure x) = GHCJSPure (f <$> x)
#endif
{-# INLINE ghcjsPureMap #-}

ghcjsPureId :: a -> GHCJSPure a
#ifdef ghcjs_HOST_OS
ghcjsPureId = id
#else
ghcjsPureId = GHCJSPure . return
#endif
{-# INLINE ghcjsPureId #-}

-- | The 'MonadJSM' is to 'JSM' what 'MonadIO' is to 'IO'.
--   When using GHCJS it is 'MonadIO'.
#ifdef ghcjs_HOST_OS
type MonadJSM = MonadIO
#else
class (Applicative m, MonadIO m) => MonadJSM m where
    liftJSM' :: JSM a -> m a

    default liftJSM' :: (MonadJSM m', MonadTrans t) => JSM a' -> t m' a'
    liftJSM' = lift . (liftJSM' :: MonadJSM m' => JSM a -> m' a)
    {-# INLINE liftJSM' #-}

instance MonadJSM JSM where
    liftJSM' = id
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (ContT r m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (Error e, MonadJSM m) => MonadJSM (ErrorT e m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (ExceptT e m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (IdentityT m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (ListT m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (MaybeT m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (ReaderT r m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (Monoid w, MonadJSM m) => MonadJSM (Lazy.RWST r w s m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (Monoid w, MonadJSM m) => MonadJSM (Strict.RWST r w s m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (Lazy.StateT s m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (MonadJSM m) => MonadJSM (Strict.StateT s m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (Monoid w, MonadJSM m) => MonadJSM (Lazy.WriterT w m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance (Monoid w, MonadJSM m) => MonadJSM (Strict.WriterT w m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance MonadRef JSM where
    type Ref JSM = Ref IO
    newRef = liftIO . newRef
    readRef = liftIO . readRef
    writeRef r = liftIO . writeRef r

instance MonadAtomicRef JSM where
    atomicModifyRef r = liftIO . atomicModifyRef r
#endif

-- | The 'liftJSM' is to 'JSM' what 'liftIO' is to 'IO'.
--   When using GHCJS it is 'liftIO'.
liftJSM :: MonadJSM m => JSM a -> m a
#ifdef ghcjs_HOST_OS
liftJSM = liftIO
#else
liftJSM = liftJSM'
#endif
{-# INLINE liftJSM #-}

-- | Type used for Haskell functions called from JavaScript.
type JSCallAsFunction = JSVal      -- ^ Function object
                     -> JSVal      -- ^ this
                     -> [JSVal]    -- ^ Function arguments
                     -> JSM ()     -- ^ Only () (aka 'JSUndefined') can be returned because
                                   --   the function may need to be executed in a
                                   --   different thread.  If you need to get a
                                   --   value out pass in a continuation function
                                   --   as an argument and invoke it from haskell.

#ifndef ghcjs_HOST_OS

class IsJSVal a where
  jsval_ :: a -> GHCJSPure JSVal

  default jsval_ :: Coercible a JSVal => a -> GHCJSPure JSVal
  jsval_ = GHCJSPure . return . coerce
  {-# INLINE jsval_ #-}

jsval :: IsJSVal a => a -> GHCJSPure JSVal
jsval = jsval_
{-# INLINE jsval #-}

data MutabilityType s = Mutable_ s
                      | Immutable_ s
                      | STMutable s

type Mutable   = Mutable_ ()
type Immutable = Immutable_ ()

data IsItMutable = IsImmutable
                 | IsMutable

type family Mutability (a :: MutabilityType s) :: IsItMutable where
  Mutability Immutable     = IsImmutable
  Mutability Mutable       = IsMutable
  Mutability (STMutable s) = IsMutable

newtype SomeJSArray (m :: MutabilityType s) = SomeJSArray JSVal
  deriving (Typeable)
instance IsJSVal (SomeJSArray m)

-- | See 'JavaScript.Array.Internal.JSArray'
type JSArray        = SomeJSArray Immutable
-- | See 'JavaScript.Array.Internal.MutableJSArray'
type MutableJSArray = SomeJSArray Mutable

-- | See 'JavaScript.Array.Internal.STJSArray'
type STJSArray s    = SomeJSArray (STMutable s)

-- | See 'JavaScript.Object.Internal.Object'
newtype Object = Object JSVal deriving(Show, ToJSON, FromJSON)

-- | See 'GHCJS.Nullable.Nullable'
newtype Nullable a = Nullable a

-- | Wrapper used when receiving a 'JSVal' from the JavaScript context
newtype JSValueReceived = JSValueReceived JSValueRef deriving(Show, ToJSON, FromJSON)

-- | Wrapper used when sending a 'JSVal' to the JavaScript context
newtype JSValueForSend = JSValueForSend JSValueRef deriving(Show, ToJSON, FromJSON, Generic)
instance NFData JSValueForSend

-- | Wrapper used when sending a 'Object' to the JavaScript context
newtype JSObjectForSend = JSObjectForSend JSValueForSend deriving(Show, ToJSON, FromJSON, Generic)
instance NFData JSObjectForSend

-- | Wrapper used when receiving a 'JSString' from the JavaScript context
newtype JSStringReceived = JSStringReceived Text deriving(Show, ToJSON, FromJSON)

-- | Wrapper used when sending a 'JString' to the JavaScript context
newtype JSStringForSend = JSStringForSend Text deriving(Show, ToJSON, FromJSON, Generic)
instance NFData JSStringForSend

-- | Command sent to a JavaScript context for execution asynchronously
data AsyncCommand = FreeRef Text JSValueForSend
                  | FreeRefs Text
                  | SetPropertyByName JSObjectForSend JSStringForSend JSValueForSend
                  | SetPropertyAtIndex JSObjectForSend Int JSValueForSend
                  | StringToValue JSStringForSend JSValueForSend
                  | NumberToValue Double JSValueForSend
                  | JSONValueToValue Value JSValueForSend
                  | GetPropertyByName JSObjectForSend JSStringForSend JSValueForSend
                  | GetPropertyAtIndex JSObjectForSend Int JSValueForSend
                  | CallAsFunction JSObjectForSend JSObjectForSend [JSValueForSend] JSValueForSend
                  | CallAsConstructor JSObjectForSend [JSValueForSend] JSValueForSend
                  | NewEmptyObject JSValueForSend
                  | NewAsyncCallback JSValueForSend
                  | NewSyncCallback JSValueForSend
                  | FreeCallback JSValueForSend
                  | NewArray [JSValueForSend] JSValueForSend
                  | EvaluateScript JSStringForSend JSValueForSend
                  | SyncWithAnimationFrame JSValueForSend
                  | StartSyncBlock
                  | EndSyncBlock
                   deriving (Show, Generic)

instance ToJSON AsyncCommand where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AsyncCommand

instance NFData AsyncCommand

-- | Command sent to a JavaScript context for execution synchronously
data Command = DeRefVal JSValueForSend
             | ValueToBool JSValueForSend
             | ValueToNumber JSValueForSend
             | ValueToString JSValueForSend
             | ValueToJSON JSValueForSend
             | ValueToJSONValue JSValueForSend
             | IsNull JSValueForSend
             | IsUndefined JSValueForSend
             | StrictEqual JSValueForSend JSValueForSend
             | InstanceOf JSValueForSend JSObjectForSend
             | PropertyNames JSObjectForSend
             | Sync
             deriving (Show, Generic)

instance ToJSON Command where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Command

instance NFData Command

-- | Batch of commands that can be sent together to the JavaScript context
data Batch = Batch [Either AsyncCommand Command] Bool Int
             deriving (Show, Generic)

instance ToJSON Batch where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Batch

instance NFData Batch

-- | Result of a 'Command' returned from the JavaScript context
data Result = DeRefValResult JSValueRef Text
            | ValueToBoolResult Bool
            | ValueToNumberResult Double
            | ValueToStringResult JSStringReceived
            | ValueToJSONResult JSStringReceived
            | ValueToJSONValueResult Value
            | IsNullResult Bool
            | IsUndefinedResult Bool
            | StrictEqualResult Bool
            | InstanceOfResult Bool
            | PropertyNamesResult [JSStringReceived]
            | ThrowJSValue JSValueReceived
            | SyncResult
            deriving (Show, Generic)

instance ToJSON Result where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Result

data BatchResults = Success [JSValueReceived] [Result]
                  | Failure [JSValueReceived] [Result] JSValueReceived
             deriving (Show, Generic)

instance ToJSON BatchResults where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON BatchResults

data Results = BatchResults Int BatchResults
             | Duplicate Int Int
             | Callback Int BatchResults JSValueReceived JSValueReceived JSValueReceived [JSValueReceived]
             | ProtocolError Text
             deriving (Show, Generic)

instance ToJSON Results where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Results
#endif

-- | Like HasCallStack, but only when jsaddle cabal flag check-unchecked is set
#if MIN_VERSION_base(4,9,0) && defined(CHECK_UNCHECKED)
type JSadddleHasCallStack = HasCallStack
#else
type JSadddleHasCallStack = (() :: Constraint)
#endif


