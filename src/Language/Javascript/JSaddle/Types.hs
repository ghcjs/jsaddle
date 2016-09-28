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
import Data.Text (Text)
import Data.Aeson
       (defaultOptions, genericToEncoding, ToJSON(..), FromJSON(..))
import GHC.Generics (Generic)
#endif

#ifdef ghcjs_HOST_OS
type JSContextRef  = ()
type Index         = Int
#else
newtype JSValueReceived = JSValueReceived Int deriving(Show, ToJSON, FromJSON)
newtype JSValueForSend = JSValueForSend Int deriving(Show, ToJSON, FromJSON)
newtype JSVal = JSVal Int deriving(Show, ToJSON, FromJSON)
newtype MutableJSArray = MutableJSArray Int deriving(Show, ToJSON, FromJSON)
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
             | NumberToValue Double
             | StringToValue JSStringForSend
             | StrictEqual JSValueForSend JSValueForSend
             | InstanceOf JSValueForSend JSObjectForSend
             | GetPropertyByName JSObjectForSend JSStringForSend
             | GetPropertyAtIndex JSObjectForSend Index
             | CallAsFunction JSObjectForSend JSObjectForSend [JSValueForSend]
             | CallAsConstructor JSObjectForSend [JSValueForSend]
             | NewEmptyObject
             | NewCallback
             | NewArray [JSValueForSend]
             | PropertyNames JSObjectForSend
             | EvaluateScript JSStringForSend
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

data Result = DeRefValResult Int Text
            | ValueToBoolResult Bool
            | ValueToNumberResult Double
            | ValueToStringResult JSStringReceived
            | ValueToJSONResult JSStringReceived
            | IsNullResult Bool
            | IsUndefinedResult Bool
            | NumberToValueResult JSValueReceived
            | StringToValueResult JSValueReceived
            | StrictEqualResult Bool
            | InstanceOfResult Bool
            | GetPropertyByNameResult JSValueReceived
            | GetPropertyAtIndexResult JSValueReceived
            | CallAsFunctionResult JSValueReceived
            | CallAsConstructorResult JSValueReceived
            | NewEmptyObjectResult JSValueReceived
            | NewCallbackResult JSValueReceived
            | Callback JSValueReceived JSValueReceived [JSValueReceived]
            | NewArrayResult JSValueReceived
            | PropertyNamesResult [JSStringReceived]
            | EvaluateScriptResult JSValueReceived
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


