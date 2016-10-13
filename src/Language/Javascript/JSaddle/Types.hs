{-# LANGUAGE CPP                        #-}
#ifdef ghcjs_HOST_OS
{-# LANGUAGE ConstraintKinds            #-}
#else
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
#endif
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

  -- * JavaScript Value Types
  , JSVal(..)
  , MutableJSArray(..)
  , Object(..)
  , JSString(..)
  , Nullable(..)
  , JSCallAsFunction

  -- * JavaScript Context Commands
#ifndef ghcjs_HOST_OS
  , JSValueReceived(..)
  , JSValueForSend(..)
  , JSStringReceived(..)
  , JSStringForSend(..)
  , JSObjectForSend(..)
  , AsyncCommand(..)
  , Command(..)
  , Batch(..)
  , Result(..)
#endif
) where

import Control.Monad.IO.Class (MonadIO(..))
#ifdef ghcjs_HOST_OS
import GHCJS.Types
import JavaScript.Object.Internal (Object(..))
import JavaScript.Array (MutableJSArray)
import Data.Word (Word(..))
import GHCJS.Nullable (Nullable(..))
#else
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State.Lazy (StateT(..))
import qualified Control.Monad.Trans.State.Strict as Strict
       (StateT(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Ref (MonadAtomicRef(..), MonadRef(..))
import Control.Concurrent.STM.TVar (TVar)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import Data.Aeson
       (defaultOptions, genericToEncoding, ToJSON(..), FromJSON(..))
import GHC.Generics (Generic)
#endif

-- | Identifies a JavaScript execution context.
--   When using GHCJS this is just '()' since their is only one context.
--   When using GHC it includes the functions JSaddle needs to communicate
--   with the JavaScript context.
#ifdef ghcjs_HOST_OS
type JSContextRef = ()
#else
data JSContextRef = JSContextRef {
    startTime          :: UTCTime
  , doSendCommand      :: Command -> IO Result
  , doSendAsyncCommand :: AsyncCommand -> IO ()
  , addCallback        :: Object -> JSCallAsFunction -> IO ()
  , freeCallback       :: Object -> IO ()
  , nextRef            :: TVar JSValueRef
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
runJSM :: JSM a -> JSContextRef -> IO a
runJSM f = const f
#else
newtype JSM a = JSM { unJSM :: ReaderT JSContextRef IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
#endif

-- | The 'MonadJSM' is to 'JSM' what 'MonadIO' is to 'IO'.
--   When using GHCJS it is 'MonadIO'.
#ifdef ghcjs_HOST_OS
type MonadJSM = MonadIO
#else
class (Applicative m, MonadIO m) => MonadJSM m where
    liftJSM' :: JSM a -> m a
    {-# MINIMAL liftJSM' #-}

instance MonadJSM JSM where
    liftJSM' = id
    {-# INLINE liftJSM' #-}

instance MonadJSM m => MonadJSM (ReaderT e m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance MonadJSM m => MonadJSM (StateT r m) where
    liftJSM' = lift . liftJSM'
    {-# INLINE liftJSM' #-}

instance MonadJSM m => MonadJSM (Strict.StateT r m) where
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
-- A reference to a particular JavaScript value inside the JavaScript context
type JSValueRef = Int64

-- | See 'GHCJS.Prim.JSVal'
newtype JSVal = JSVal JSValueRef deriving(Show, ToJSON, FromJSON)

-- | See 'JavaScript.Array.Internal.MutableJSArray'
newtype MutableJSArray = MutableJSArray JSValueRef deriving(Show, ToJSON, FromJSON)

-- | See 'JavaScript.Object.Internal.Object'
newtype Object = Object JSVal deriving(Show, ToJSON, FromJSON)

-- | See 'GHCJS.Nullable.Nullable'
newtype Nullable a = Nullable a

-- | See 'Data.JSString.Internal.Type'
newtype JSString = JSString Text deriving(Show, ToJSON, FromJSON)

-- | Wrapper used when receiving a 'JSVal' from the JavaScript context
newtype JSValueReceived = JSValueReceived JSValueRef deriving(Show, ToJSON, FromJSON)

-- | Wrapper used when sending a 'JSVal' to the JavaScript context
newtype JSValueForSend = JSValueForSend JSValueRef deriving(Show, ToJSON, FromJSON)

-- | Wrapper used when sending a 'Object' to the JavaScript context
newtype JSObjectForSend = JSObjectForSend JSValueForSend deriving(Show, ToJSON, FromJSON)

-- | Wrapper used when receiving a 'JSString' from the JavaScript context
newtype JSStringReceived = JSStringReceived Text deriving(Show, ToJSON, FromJSON)

-- | Wrapper used when sending a 'JString' to the JavaScript context
newtype JSStringForSend = JSStringForSend Text deriving(Show, ToJSON, FromJSON)

-- | Command sent to a JavaScript context for execution asynchronously
data AsyncCommand = FreeRef JSValueForSend
                  | SetPropertyByName JSObjectForSend JSStringForSend JSValueForSend
                  | SetPropertyAtIndex JSObjectForSend Int JSValueForSend
                  | StringToValue JSStringForSend JSValueForSend
                  | NumberToValue Double JSValueForSend
                  | GetPropertyByName JSObjectForSend JSStringForSend JSValueForSend
                  | GetPropertyAtIndex JSObjectForSend Int JSValueForSend
                  | CallAsFunction JSObjectForSend JSObjectForSend [JSValueForSend] JSValueForSend
                  | CallAsConstructor JSObjectForSend [JSValueForSend] JSValueForSend
                  | NewEmptyObject JSValueForSend
                  | NewCallback JSValueForSend
                  | NewArray [JSValueForSend] JSValueForSend
                  | EvaluateScript JSStringForSend JSValueForSend
                  | SyncWithAnimationFrame JSValueForSend
             deriving (Show, Generic)

instance ToJSON AsyncCommand where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AsyncCommand

-- | Command sent to a JavaScript context for execution synchronously
data Command = DeRefVal JSValueForSend
             | ValueToBool JSValueForSend
             | ValueToNumber JSValueForSend
             | ValueToString JSValueForSend
             | ValueToJSON JSValueForSend
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

-- | Batch of commands that can be sent together to the JavaScript context
data Batch = Batch [AsyncCommand] Command Bool
             deriving (Show, Generic)

instance ToJSON Batch where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Batch

-- | Result of a 'Command' returned from the JavaScript context
data Result = DeRefValResult JSValueRef Text
            | ValueToBoolResult Bool
            | ValueToNumberResult Double
            | ValueToStringResult JSStringReceived
            | ValueToJSONResult JSStringReceived
            | IsNullResult Bool
            | IsUndefinedResult Bool
            | StrictEqualResult Bool
            | InstanceOfResult Bool
            | Callback JSValueReceived JSValueReceived [JSValueReceived]
            | PropertyNamesResult [JSStringReceived]
            | ThrowJSValue JSValueReceived
            | ProtocolError Text
            | SyncResult
             deriving (Show, Generic)

instance ToJSON Result where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Result
#endif



