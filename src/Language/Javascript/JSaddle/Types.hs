{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Properties
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Types (
    JSVal(..)
  , MutableJSArray(..)
  , Object(..)
  , JSContextRef(..)
  , JSString(..)
  , Index
  , Nullable(..)
  , JSM
  , JSCallAsFunction
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

import Control.Monad.Trans.Reader (ReaderT(..))
#ifdef ghcjs_HOST_OS
import GHCJS.Types
import JavaScript.Object.Internal (Object(..))
import JavaScript.Array (MutableJSArray)
import Data.Word (Word(..))
import GHCJS.Nullable (Nullable(..))
#else
import Data.Int (Int64)
import Data.Text (Text)
import Data.Aeson
       (defaultOptions, genericToEncoding, ToJSON(..), FromJSON(..))
import GHC.Generics (Generic)
import Control.Concurrent.STM.TVar (TVar)
#endif

#ifdef ghcjs_HOST_OS
type JSContextRef  = ()
type Index         = Int
#else
type JSRef = Int64
newtype JSValueReceived = JSValueReceived JSRef deriving(Show, ToJSON, FromJSON)
newtype JSValueForSend = JSValueForSend JSRef deriving(Show, ToJSON, FromJSON)
newtype JSVal = JSVal JSRef deriving(Show, ToJSON, FromJSON)
newtype MutableJSArray = MutableJSArray JSRef deriving(Show, ToJSON, FromJSON)
type Index = Int
newtype JSObjectForSend = JSObjectForSend JSValueForSend deriving(Show, ToJSON, FromJSON)
newtype Object = Object JSVal deriving(Show, ToJSON, FromJSON)
newtype JSStringReceived = JSStringReceived Text deriving(Show, ToJSON, FromJSON)
newtype JSStringForSend = JSStringForSend Text deriving(Show, ToJSON, FromJSON)
newtype JSString = JSString Text deriving(Show, ToJSON, FromJSON)
newtype Nullable a = Nullable a

data AsyncCommand = FreeRef JSValueForSend
                  | SetPropertyByName JSObjectForSend JSStringForSend JSValueForSend
                  | SetPropertyAtIndex JSObjectForSend Index JSValueForSend
                  | StringToValue JSStringForSend JSValueForSend
                  | NumberToValue Double JSValueForSend
                  | GetPropertyByName JSObjectForSend JSStringForSend JSValueForSend
                  | GetPropertyAtIndex JSObjectForSend Index JSValueForSend
                  | CallAsFunction JSObjectForSend JSObjectForSend [JSValueForSend] JSValueForSend
                  | CallAsConstructor JSObjectForSend [JSValueForSend] JSValueForSend
                  | NewEmptyObject JSValueForSend
                  | NewCallback JSValueForSend
                  | NewArray [JSValueForSend] JSValueForSend
                  | EvaluateScript JSStringForSend JSValueForSend
             deriving (Show, Generic)

instance ToJSON AsyncCommand where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AsyncCommand

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

data Batch = Batch [AsyncCommand] Command
             deriving (Show, Generic)

instance ToJSON Batch where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Batch

data Result = DeRefValResult JSRef Text
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

data JSContextRef = JSContextRef {
    doSendCommand :: Command -> IO Result
  , doSendAsyncCommand :: AsyncCommand -> IO ()
  , addCallback :: Object -> JSCallAsFunction -> IO ()
  , freeCallback :: Object -> IO ()
  , nextRef :: TVar JSRef
}

#endif

-- | Type used for Haskell functions called from JavaScript.
type JSCallAsFunction = JSVal      -- ^ Function object
                     -> JSVal      -- ^ this
                     -> [JSVal]    -- ^ Function arguments
                     -> JSM ()     -- ^ Only () (aka 'JSUndefined') can be returned because
                                   --   the function may need to be executed in a
                                   --   different thread.  If you need to get a
                                   --   value out pass in a continuation function
                                   --   as an argument and invoke it from haskell.

-- | The @JSM@ monad keeps track of the JavaScript context.
--
-- Given a @JSM@ function and a 'JSContextRef' you can run the
-- function like this...
--
-- > runReaderT jsmFunction javaScriptContext
--
-- For an example of how to set up WebKitGTK+ see tests/TestJSaddle.hs
type JSM = ReaderT JSContextRef IO


