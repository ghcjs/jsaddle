-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.WebSockets
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.WebSockets (
    Command(..)
  , Result(..)
  , sendCommand
  , sendAsyncCommand
) where

import Data.Text (Text)
import Language.Javascript.JSaddle.Types
       (JSStringReceived(..), JSStringForSend, JSValueForSend,
        JSValueReceived(..), JSObjectForSend, Index)
import Language.Javascript.JSaddle.Monad (JSM)

data Command = JSStringToText JSStringForSend
             | TextToJSString Text
             | DeRefVal JSValueForSend
             | ValueToBool JSValueForSend
             | ValueToNumber JSValueForSend
             | ValueToString JSValueForSend
             | ValueToJSON JSValueForSend
             | NumberToValue Double
             | StrictEqual JSValueForSend JSValueForSend
             | InstanceOf JSValueForSend JSObjectForSend
             | GetPropertyByName JSObjectForSend JSStringForSend
             | GetPropertyAtIndex JSObjectForSend Index
             | SetPropertyByName JSObjectForSend JSStringForSend JSValueForSend
             | SetPropertyAtIndex JSObjectForSend Index JSValueForSend
             | CallAsFunction JSObjectForSend JSObjectForSend [JSValueForSend]
             | CallAsConstructor JSObjectForSend [JSValueForSend]
             | NewEmptyObject
             | NewCallback
             | NewArray [JSValueForSend]
             | PropertyNames JSObjectForSend
             | EvaluateScript JSStringForSend
data Result = JSStringToTextResult Text
            | TextToJSStringResult JSStringReceived
            | DeRefValResult Int Text
            | ValueToBoolResult Bool
            | ValueToNumberResult Double
            | ValueToStringResult JSStringReceived
            | ValueToJSONResult JSStringReceived
            | NumberToValueResult JSValueReceived
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

sendCommand :: Command -> JSM Result
sendCommand = undefined

sendAsyncCommand :: Command -> JSM ()
sendAsyncCommand = undefined

