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
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations      #-}
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

  -- * The JSM Monad
    JSM(..)
  , MonadJSM(..)
  , liftJSM
  , askJSM

  -- * pure GHCJS functions
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

#ifndef ghcjs_HOST_OS

  -- * JavaScript Context Commands
  , MutabilityType(..)
  , Mutable
  , Immutable
  , IsItMutable(..)
  , Mutability

  , JSContextRef (..)
  , Req (..)
  , Val
  , ValId
  , Ref (..)
  , RefId (..)
  , LazyVal (..)
  , lazyValFromStrict
  , getLazyVal
  , Rsp (..)
  , SyncCommand (..)
  , CallbackId (..)
  , GetJsonReqId (..)
  , SyncReqId (..)
  , TryId (..)
  , PrimVal (..)
  , TryReq (..)
  , SyncState (..)
  , JavaScriptException (..)
  , runJSM
  , sync
  , freeSyncCallback
  , newSyncCallback'
  , newSyncCallback''
  , withJSValId
  , wrapJSVal
  , newJson
  , lazyValResult
  , callbackToSyncFunction
  , callbackToAsyncFunction
  , getProperty
  , setProperty
  , getJson
  , getJsonLazy
  , callAsFunction'
  , callAsConstructor'
  , withLog
  , unsafeInlineLiftIO
#endif
) where

import Control.Monad.IO.Class (MonadIO(..))
#ifdef ghcjs_HOST_OS
import GHCJS.Types
import JavaScript.Object.Internal (Object(..))
import JavaScript.Array.Internal (SomeJSArray(..), JSArray, MutableJSArray, STJSArray)
import GHCJS.Nullable (Nullable(..))
#else
import GHCJS.Prim.Internal
import Data.JSString.Internal.Type (JSString(..))
import Data.Monoid
import Control.Concurrent (myThreadId, ThreadId, threadDelay)
import Control.Exception (Exception, throwIO)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow, MonadCatch(..), MonadMask(..), bracket_)
import Control.Monad.Except
import Control.Monad.Trans.Cont (ContT(..))
import Control.Monad.Trans.Error (Error(..), ErrorT(..))
import Control.Monad.Trans.Except (ExceptT(..))
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.List (ListT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Control.Monad.Trans.RWS.Lazy as Lazy (RWST(..))
import Control.Monad.Trans.RWS.Strict as Strict (RWST(..))
import Control.Monad.Trans.State.Lazy as Lazy (StateT(..))
import Control.Monad.Trans.State.Strict as Strict (StateT(..))
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT(..))
import Control.Monad.Trans.Writer.Strict as Strict (WriterT(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Fix (MonadFix)
import Control.Concurrent.Async (withAsync, wait)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.MVar (MVar, swapMVar, modifyMVar, readMVar)
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Typeable (Typeable)
import Data.Coerce (coerce, Coercible)
import Data.Aeson (ToJSON(..), FromJSON(..))
import GHC.Generics (Generic)
import Data.Int
import qualified Data.Aeson as A
import Data.Map (Map)
import System.IO.Unsafe
import Data.IORef
import Control.Monad.Ref (MonadRef, MonadAtomicRef(..))
import qualified Control.Monad.Ref as MonadRef
import Control.Concurrent.MVar
       (putMVar, takeMVar, newEmptyMVar)
import qualified Data.Map as M
import Control.Monad.Trans.Reader (asks, runReaderT)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar
       (writeTVar, readTVar, modifyTVar')
import Control.Monad.Primitive
import Control.Monad.IO.Unlift (MonadUnliftIO(..), UnliftIO(..))
#endif

#if MIN_VERSION_base(4,9,0) && defined(CHECK_UNCHECKED)
import GHC.Stack (HasCallStack)
#else
import GHC.Exts (Constraint)
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

    default liftJSM' :: (MonadJSM m', MonadTrans t, m ~ t m') => JSM a -> m a
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

type Mutable   = 'Mutable_ ()
type Immutable = 'Immutable_ ()

data IsItMutable = IsImmutable
                 | IsMutable

type family Mutability (a :: MutabilityType s) :: IsItMutable where
  Mutability Immutable     = 'IsImmutable
  Mutability Mutable       = 'IsMutable
  Mutability ('STMutable s) = 'IsMutable

newtype SomeJSArray (m :: MutabilityType s) = SomeJSArray JSVal
  deriving (Typeable)
instance IsJSVal (SomeJSArray m)

-- | See 'JavaScript.Array.Internal.JSArray'
type JSArray        = SomeJSArray Immutable
-- | See 'JavaScript.Array.Internal.MutableJSArray'
type MutableJSArray = SomeJSArray Mutable

-- | See 'JavaScript.Array.Internal.STJSArray'
type STJSArray s    = SomeJSArray ('STMutable s)

-- | See 'JavaScript.Object.Internal.Object'
newtype Object = Object JSVal

-- | See 'GHCJS.Nullable.Nullable'
newtype Nullable a = Nullable a
#endif

-- | Like HasCallStack, but only when jsaddle cabal flag check-unchecked is set
#if MIN_VERSION_base(4,9,0) && defined(CHECK_UNCHECKED)
type JSadddleHasCallStack = HasCallStack
#else
type JSadddleHasCallStack = (() :: Constraint)
#endif


--TODO: We know what order we issued SyncBlock, GetJson, etc. requests in, so we can probably match them up without explicit IDs

newtype GetJsonReqId = GetJsonReqId { unGetJsonReqId :: Int64 } deriving (Show, Read, Eq, Ord, Enum, ToJSON, FromJSON)

newtype CallbackId = CallbackId { unCallbackId :: Int64 } deriving (Show, Read, Eq, Ord, Enum, ToJSON, FromJSON)

newtype TryId = TryId { unTryId :: Int64 } deriving (Show, Read, Eq, Ord, Enum, ToJSON, FromJSON)

newtype SyncReqId = SyncReqId { unSyncReqId :: Int64 } deriving (Show, Read, Eq, Ord, Enum, ToJSON, FromJSON)

aesonOptions :: String -> A.Options
aesonOptions typeName = A.defaultOptions
  { A.constructorTagModifier = drop (length typeName + 1)
  , A.fieldLabelModifier = drop (length typeName + 2)
  }

--TODO: Make outputs optional
data Req input output
   = Req_FreeRef RefId --TODO: No thread IDs for these
   | Req_NewJson A.Value output
   | Req_GetJson input GetJsonReqId
   | Req_SyncBlock CallbackId -- ^ Ask JS to begin a synchronous block --TODO: Should we get rid of this and just use NewSyncCallback/CallAsFunction?
   | Req_NewSyncCallback CallbackId output -- ^ Create a new sync callback; note that we don't inform the JS side when we dispose of a callback - it's an error to call a disposed callback, so we just detect and throw that error on the Haskell side
   | Req_NewAsyncCallback CallbackId output -- ^ Create a new async callback; note that we don't inform the JS side when we dispose of a callback - it's an error to call a disposed callback, so we just detect and throw that error on the Haskell side
   | Req_SetProperty input input input -- ^ @Req_SetProperty a b c@ ==> @c[a] = b;@
   | Req_GetProperty input input output -- ^ @Req_SetProperty a b c@ ==> @c = b[a];@
   | Req_CallAsFunction input input [input] output
   | Req_CallAsConstructor input [input] output
   | Req_Throw input
   | Req_FinishTry
   | Req_Sync SyncReqId
   deriving (Show, Read, Eq, Generic, Functor, Foldable, Traversable)

instance Bifunctor Req where
  bimap = bimapDefault

instance Bifoldable Req where
  bifoldMap = bifoldMapDefault

instance Bitraversable Req where
  bitraverse f g = \case
    Req_FreeRef a -> Req_FreeRef <$> pure a
    Req_NewJson a b -> Req_NewJson <$> pure a <*> g b
    Req_GetJson a b -> Req_GetJson <$> f a <*> pure b
    Req_SyncBlock a -> Req_SyncBlock <$> pure a
    Req_NewSyncCallback a b -> Req_NewSyncCallback <$> pure a <*> g b
    Req_NewAsyncCallback a b -> Req_NewAsyncCallback <$> pure a <*> g b
    Req_SetProperty a b c -> Req_SetProperty <$> f a <*> f b <*> f c
    Req_GetProperty a b c -> Req_GetProperty <$> f a <*> f b <*> g c
    Req_CallAsFunction a b c d -> Req_CallAsFunction <$> f a <*> f b <*> traverse f c <*> g d
    Req_CallAsConstructor a b c -> Req_CallAsConstructor <$> f a <*> traverse f b <*> g c
    Req_Throw a -> Req_Throw <$> f a
    Req_FinishTry -> pure Req_FinishTry
    Req_Sync s -> pure $ Req_Sync s

instance (ToJSON input, ToJSON output) => ToJSON (Req input output) where
  toEncoding = A.genericToEncoding $ aesonOptions "Req"

instance (FromJSON input, FromJSON output) => FromJSON (Req input output) where
  parseJSON = A.genericParseJSON $ aesonOptions "Req"

data Rsp
   = Rsp_GetJson GetJsonReqId A.Value
   | Rsp_Result RefId (PrimVal ())
   | Rsp_CallAsync CallbackId ValId [ValId]
   --TODO: When an exception is thrown, make sure we stop waiting for any results from them; otherwise, the datastructures waiting for those results will leak
   | Rsp_FinishTry TryId (Either ValId ()) -- Left if an exception was thrown; Right if not
   | Rsp_Sync SyncReqId
   deriving (Show, Read, Eq, Generic)

instance ToJSON Rsp where
  toEncoding = A.genericToEncoding $ aesonOptions "Rsp"

instance FromJSON Rsp where
  parseJSON = A.genericParseJSON $ aesonOptions "Rsp"

data SyncCommand
   = SyncCommand_StartCallback CallbackId ValId [ValId] -- The input valIds here must always be allocated on the JS side --TODO: Make sure throwing stuff works when it ends up skipping over our own call stack entries
   | SyncCommand_Continue
   deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON SyncCommand where
  toEncoding = A.genericToEncoding $ aesonOptions "SyncCommand"

instance FromJSON SyncCommand where
  parseJSON = A.genericParseJSON $ aesonOptions "SyncCommand"

data TryReq = TryReq
  { _tryReq_tryId :: TryId
  , _tryReq_req :: Req ValId RefId
  }
  deriving (Generic)

instance ToJSON TryReq where
  toEncoding = A.genericToEncoding $ aesonOptions "TryReq"

instance FromJSON TryReq where
  parseJSON = A.genericParseJSON $ aesonOptions "TryReq"

data JSContextRef = JSContextRef
  { _jsContextRef_sendReq :: !(TryReq -> IO ())
  , _jsContextRef_sendReqAsync :: !(TryReq -> IO ())
  , _jsContextRef_syncThreadId :: Maybe (ThreadId)
  , _jsContextRef_nextRefId :: !(TVar RefId)
  , _jsContextRef_nextGetJsonReqId :: !(TVar GetJsonReqId)
  , _jsContextRef_getJsonReqs :: !(TVar (Map GetJsonReqId (MVar A.Value))) -- ^ The GetJson requests that are currently in-flight
  , _jsContextRef_nextCallbackId :: !(TVar CallbackId)
  , _jsContextRef_callbacks :: !(TVar (Map CallbackId (JSVal -> [JSVal] -> JSM JSVal)))
  , _jsContextRef_pendingResults :: !(TVar (Map RefId (MVar (PrimVal ()))))
  , _jsContextRef_nextTryId :: !(TVar TryId)
  , _jsContextRef_tries :: !(TVar (Map TryId (MVar (Either JSVal ()))))
  , _jsContextRef_myTryId :: !TryId
  , _jsContextRef_syncState :: !(MVar SyncState)
  , _jsContextRef_nextSyncReqId :: !(TVar SyncReqId)
  , _jsContextRef_syncReqs :: !(TVar (Map SyncReqId (MVar ())))
  }

-- | Perform IO from JSM without synchronizing with the JS side; since requests
-- are heavily pipelined, this may result in unpredictable ordering of IO and JS
-- operations, even within a single thread
unsafeInlineLiftIO :: IO a -> JSM a
unsafeInlineLiftIO = JSM . liftIO

sync :: JSM a -> JSM a
sync syncBlock = do
  resultVar <- JSM $ liftIO newEmptyMVar
  (cb, f) <- newSyncCallback' $ \_ _ _ -> do
    JSM . liftIO . putMVar resultVar =<< syncBlock
  _ <- callAsFunction' f (primToJSVal PrimVal_Null) []
  result <- JSM $ liftIO $ takeMVar resultVar
  freeSyncCallback cb
  return result

newSyncCallback' :: JSCallAsFunction -> JSM (CallbackId, JSVal)
newSyncCallback' f = newSyncCallback'' $ \fObj this args -> primToJSVal PrimVal_Undefined <$ f fObj this args

newSyncCallback''
  :: (  JSVal      -- Function object
     -> JSVal      -- this
     -> [JSVal]    -- Function arguments
     -> JSM JSVal)
  -> JSM (CallbackId, JSVal)
newSyncCallback'' f = do
  callbackId <- newId _jsContextRef_nextCallbackId
  f' <- callbackToSyncFunction callbackId --TODO: "ContinueAsync" behavior
  callbacks <- JSM $ asks _jsContextRef_callbacks
  JSM $ liftIO $ atomically $ modifyTVar' callbacks $ M.insertWith (error "newSyncCallback: callbackId already exists") callbackId $ \this args -> f f' this args
  return (callbackId, f')

freeSyncCallback :: CallbackId -> JSM ()
freeSyncCallback cbid = do
  callbacks <- JSM $ asks _jsContextRef_callbacks
  --TODO: Only do this once it actually comes back
  --TODO: Don't fully synchronize here; just hold onto it until the frontend approves
  liftIO $ atomically $ modifyTVar' callbacks $ M.delete cbid

newId :: Enum a => (JSContextRef -> TVar a) -> JSM a
newId f = JSM $ do
  v <- asks f
  liftIO $ getNextTVar v

getNextTVar :: Enum a => TVar a -> IO a
getNextTVar v = atomically $ do
  a <- readTVar v
  -- Evaluate this strictly so that thunks cannot build up
  writeTVar v $! succ a
  return a

sendReq :: Req JSVal Ref -> JSM ()
sendReq req = withReqId req sendReqId

sendReqId :: Req ValId RefId -> JSM ()
sendReqId r = JSM $ do
  s <- asks _jsContextRef_sendReq
  tid <- asks _jsContextRef_myTryId
  syncState <- asks _jsContextRef_syncState
  _ <- liftIO $ swapMVar syncState SyncState_OutOfSync
  liftIO $ s $ TryReq
    { _tryReq_tryId = tid
    , _tryReq_req = r
    }

lazyValToVal :: LazyVal -> JSM Val
lazyValToVal val = do
  JSM (liftIO (readIORef $ _lazyVal_ref val)) >>= \case
    Nothing -> return $ _lazyVal_val val
    Just ref -> return $ PrimVal_Ref ref

withReqId :: Req JSVal Ref -> (Req ValId RefId -> JSM a) -> JSM a
withReqId req = runContT $ do
  -- Use ContT to apply all the relevant with* functions in a nested fashion
  bitraverse (ContT . withJSValId) (ContT . withRefId) req

callbackToSyncFunction :: CallbackId -> JSM JSVal
callbackToSyncFunction callbackId = withJSValOutput_ $ \ref -> do
  sendReq $ Req_NewSyncCallback callbackId ref

callbackToAsyncFunction :: CallbackId -> JSM JSVal
callbackToAsyncFunction callbackId = withJSValOutput_ $ \ref -> do
  sendReq $ Req_NewAsyncCallback callbackId ref

--TODO: This *MUST* be run before sendReq; we should change the type to enforce this
lazyValResult :: Ref -> JSM LazyVal
lazyValResult ref = JSM $ do
  pendingResults <- asks _jsContextRef_pendingResults
  liftIO $ do
    refId <- readIORef $ unRef ref
    resultVar <- newEmptyMVar
    atomically $ modifyTVar' pendingResults $ M.insertWith (error "getLazyVal: already waiting for this ref") refId resultVar
    refRef <- newIORef $ Just ref
    resultVal <- unsafeInterleaveIO $ do
      result <- takeMVar resultVar
      writeIORef refRef Nothing
      return result
    return $ LazyVal
      { _lazyVal_ref = refRef
      , _lazyVal_val = ref <$ resultVal
      }

newRef :: JSM Ref
newRef = do
  -- Bind this strictly in case it would retain anything else in the finalizer
  !valId <- newId _jsContextRef_nextRefId
  wrapRef valId

wrapRef :: RefId -> JSM Ref
wrapRef valId = JSM $ do
  valRef <- liftIO $ newIORef valId
  -- Bind this strictly to avoid retaining the whole JSContextRef in the finalizer
  !sendReq' <- asks _jsContextRef_sendReq
  void $ liftIO $ mkWeakIORef valRef $ do
    sendReq' $ TryReq
      { _tryReq_tryId = TryId 0 --TODO: This probably shouldn't even be a TryReq
      , _tryReq_req = Req_FreeRef valId
      }
  return $ Ref valRef

-- | Run a computation with the given RefId available; the value will not be freed during this computation
--
-- WARNING: Do not allow the RefId to escape the scope, or it may be freed while a reference still exists
withRefId :: Ref -> (RefId -> JSM a) -> JSM a
withRefId val f = do
  valId <- JSM $ liftIO $ readIORef $ unRef val
  result <- f valId
  JSM $ liftIO $ touch val -- Ensure that the value is not freed before the end of the action
  return result

wrapVal :: ValId -> JSM Val
wrapVal = traverse wrapRef

wrapJSVal :: ValId -> JSM JSVal
wrapJSVal valId = primToJSVal <$> wrapVal valId

withJSValId :: JSVal -> (ValId -> JSM a) -> JSM a
withJSValId (JSVal lv) f = do
  val <- lazyValToVal lv
  withValId val f

withValId :: Val -> (ValId -> JSM a) -> JSM a
withValId val f = case val of
  PrimVal_Undefined -> f PrimVal_Undefined
  PrimVal_Null -> f PrimVal_Null
  PrimVal_Bool b -> f $ PrimVal_Bool b
  PrimVal_Number n -> f $ PrimVal_Number n
  PrimVal_String s -> f $ PrimVal_String s
  PrimVal_Ref r -> withRefId r $ f . PrimVal_Ref

getJson :: JSVal -> JSM A.Value
getJson val = do
  getResult <- getJson' val
  JSM $ liftIO getResult

getJsonLazy :: JSVal -> JSM A.Value
getJsonLazy val = do
  getResult <- getJson' val
  JSM $ liftIO $ unsafeInterleaveIO getResult

getJson' :: JSVal -> JSM (IO A.Value)
getJson' val = do
  getJsonReqId <- newId _jsContextRef_nextGetJsonReqId
  getJsonReqs <- JSM $ asks _jsContextRef_getJsonReqs
  resultVar <- JSM $ liftIO $ newEmptyMVar
  JSM $ liftIO $ atomically $ modifyTVar' getJsonReqs $ M.insert getJsonReqId resultVar
  sendReq $ Req_GetJson val getJsonReqId
  return $ takeMVar resultVar

withJSValOutput_ :: (Ref -> JSM ()) -> JSM JSVal
withJSValOutput_ f = do
  ref <- newRef
  result <- JSVal <$> lazyValResult ref
  f ref
  return result

newJson :: A.Value -> JSM JSVal
newJson v = withJSValOutput_ $ \ref -> do
  sendReq $ Req_NewJson v ref

getProperty :: JSVal -> JSVal -> JSM JSVal
getProperty prop obj = withJSValOutput_ $ \ref -> do
  sendReq $ Req_GetProperty prop obj ref

setProperty :: JSVal -> JSVal -> JSVal -> JSM ()
setProperty prop val obj = do
  sendReq $ Req_SetProperty prop val obj

callAsFunction' :: JSVal -> JSVal -> [JSVal] -> JSM JSVal
callAsFunction' f this args = withJSValOutput_ $ \ref -> do
  sendReq $ Req_CallAsFunction f this args ref

callAsConstructor' :: JSVal -> [JSVal] -> JSM JSVal
callAsConstructor' f args = withJSValOutput_ $ \ref -> do
  sendReq $ Req_CallAsConstructor f args ref

newtype JSM a = JSM { unJSM :: ReaderT JSContextRef IO a } deriving (Functor, Applicative, Monad, MonadFix, MonadThrow)

--Exceptions: it's pointless for Haskell-side to throw exceptions to javascript side asynchronously, but perhaps it should be allowed
instance MonadError JSVal JSM where
  catchError a h = do
    tryId <- newId _jsContextRef_nextTryId
    tries <- JSM $ asks _jsContextRef_tries
    finishVar <- JSM $ liftIO newEmptyMVar
    JSM $ liftIO $ atomically $ modifyTVar' tries $ M.insert tryId finishVar
    env <- JSM ask
    let end = sendReq Req_FinishTry
    tryResult <- JSM $ liftIO $ withAsync (runReaderT (unJSM (a <* end)) $ env { _jsContextRef_myTryId = tryId }) $ \aa -> do
      --IDEA: Continue speculatively executing forward
      takeMVar finishVar >>= \case
        Left e -> return $ Left e
        Right _ -> Right <$> wait aa
    case tryResult of
      Left e -> h e
      Right result -> return result
  throwError e = do
    sendReq $ Req_Throw e --IDEA: Short-circuit this rather than actually sending it to the JS side
    JSM $ liftIO $ forever $ threadDelay maxBound

instance MonadIO JSM where
  liftIO a = do
    waitForSync
    JSM $ liftIO a

instance MonadUnliftIO JSM where
  askUnliftIO = do
    ctx <- askJSM
    return $ UnliftIO (`runJSM` ctx)

instance MonadRef JSM where
    type Ref JSM = MonadRef.Ref IO
    --TODO: Can we do any of these things without synchronizing with JS?
    newRef = liftIO . MonadRef.newRef
    readRef = liftIO . MonadRef.readRef
    writeRef r = liftIO . MonadRef.writeRef r

instance MonadAtomicRef JSM where
    atomicModifyRef r = liftIO . MonadRef.atomicModifyRef r

--TODO: Figure out what syncAfter was doing in MonadCatch and MonadMask, and do that
instance MonadCatch JSM where
    t `catch` c = JSM (unJSM t `catch` \e -> unJSM (c e))

instance MonadMask JSM where
  mask a = JSM $ mask $ \unmask -> unJSM (a $ q unmask)
    where q :: (ReaderT JSContextRef IO a -> ReaderT JSContextRef IO a) -> JSM a -> JSM a
          q unmask (JSM b) = JSM $ unmask b
  uninterruptibleMask a =
    JSM $ uninterruptibleMask $ \unmask -> unJSM (a $ q unmask)
      where q :: (ReaderT JSContextRef IO a -> ReaderT JSContextRef IO a) -> JSM a -> JSM a
            q unmask (JSM b) = JSM $ unmask b

newtype JavaScriptException = JavaScriptException { unJavaScriptException :: JSVal }
  deriving (Typeable)

instance Show JavaScriptException where
  show _ = "JavaScriptException _"

instance Exception JavaScriptException

runJSM :: MonadIO m => JSM a -> JSContextRef -> m a
runJSM a ctx = liftIO $ do
  threadId <- myThreadId
  let ctx' = if _jsContextRef_syncThreadId ctx == Just threadId
                then ctx
                else ctx {
                    _jsContextRef_syncThreadId = Nothing,
                    _jsContextRef_sendReq = _jsContextRef_sendReqAsync ctx }
  result <- flip runReaderT ctx' $ unJSM $ do
    catchError (Right <$> a) (return . Left)  -- <* waitForSync
  either (throwIO . JavaScriptException) return result

askJSM :: MonadJSM m => m JSContextRef
askJSM = liftJSM $ JSM ask

withLog :: String -> IO a -> IO a
withLog s = bracket_ (putStrLn $ s <> ": enter") (putStrLn $ s <> ": exit")

data SyncState
   = SyncState_InSync -- We are in sync with the JS engine
   | SyncState_OutOfSync -- We are out of sync with the JS engine, but we have not started synchronizing
   | SyncState_WaitingForSync (MVar ()) -- We are in the process of synchronizing with the JS engine; this MVar will get filled in when we finish

waitForSync :: JSM ()
waitForSync = do
  syncState <- JSM $ asks _jsContextRef_syncState
  syncReqs <- JSM $ asks _jsContextRef_syncReqs
  nextSyncReqId <- JSM $ asks _jsContextRef_nextSyncReqId
  sendReq' <- JSM $ asks _jsContextRef_sendReq
  tid <- JSM $ asks _jsContextRef_myTryId
  join $ unsafeInlineLiftIO $ modifyMVar syncState $ \old -> case old of
    SyncState_InSync -> return (old, return ())
    SyncState_OutOfSync -> do
      synced <- newEmptyMVar
      syncReqId <- getNextTVar nextSyncReqId
      atomically $ modifyTVar' syncReqs $ M.insert syncReqId synced
      sendReq' TryReq
        { _tryReq_tryId = tid
        , _tryReq_req = Req_Sync syncReqId
        }
      return $ (,) (SyncState_WaitingForSync synced) $
        unsafeInlineLiftIO $ readMVar synced
    SyncState_WaitingForSync synced -> do
      return $ (,) old $
        unsafeInlineLiftIO $ readMVar synced
