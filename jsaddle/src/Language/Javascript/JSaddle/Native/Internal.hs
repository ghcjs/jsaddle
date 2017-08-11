{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Native
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Native.Internal (
    wrapJSVal
  , wrapJSString
  , withJSVal
  , withJSVals
  , withObject
  , withJSString
  , setPropertyByName
  , setPropertyAtIndex
  , stringToValue
  , numberToValue
  , jsonValueToValue
  , getPropertyByName
  , getPropertyAtIndex
  , callAsFunction
  , callAsConstructor
  , newEmptyObject
  , newAsyncCallback
  , newSyncCallback
  , newArray
  , evaluateScript
  , deRefVal
  , valueToBool
  , valueToNumber
  , valueToString
  , valueToJSON
  , valueToJSONValue
  , isNull
  , isUndefined
  , strictEqual
  , instanceOf
  , propertyNames
) where

import Control.Monad.IO.Class (MonadIO(..))

import Data.Aeson (Value)

import Language.Javascript.JSaddle.Types
       (AsyncCommand(..), JSM(..), JSString(..), addCallback,
        Object(..), JSVal(..), JSValueForSend(..), JSCallAsFunction,
        JSStringReceived(..), JSStringForSend(..), JSObjectForSend(..))
import Language.Javascript.JSaddle.Monad (askJSM)
import Language.Javascript.JSaddle.Run
       (Command(..), Result(..), sendCommand,
        sendAsyncCommand, sendLazyCommand, wrapJSVal)
import GHC.IORef (IORef(..), readIORef)
import GHC.STRef (STRef(..))
import GHC.IO (IO(..))
import GHC.Base (touch#)

wrapJSString :: MonadIO m => JSStringReceived -> m JSString
wrapJSString (JSStringReceived ref) = return $ JSString ref

touchIORef :: IORef a -> IO ()
touchIORef (IORef (STRef r#)) = IO $ \s -> case touch# r# s of s' -> (# s', () #)

withJSVal :: MonadIO m => JSVal -> (JSValueForSend -> m a) -> m a
withJSVal (JSVal ref) f = do
    result <- (f . JSValueForSend) =<< liftIO (readIORef ref)
    liftIO $ touchIORef ref
    return result

withJSVals :: MonadIO m => [JSVal] -> ([JSValueForSend] -> m a) -> m a
withJSVals v f =
 do result <- f =<< mapM (\(JSVal ref) -> liftIO $ JSValueForSend <$> readIORef ref) v
    liftIO $ mapM_ (\(JSVal ref) -> touchIORef ref) v
    return result

withObject :: MonadIO m => Object -> (JSObjectForSend -> m a) -> m a
withObject (Object o) f = withJSVal o (f . JSObjectForSend)

withJSString :: MonadIO m => JSString -> (JSStringForSend -> m a) -> m a
withJSString (JSString ref) f = f (JSStringForSend ref)

setPropertyByName :: JSString -> JSVal -> Object -> JSM ()
setPropertyByName name val this =
    withObject this $ \rthis ->
        withJSString name $ \rname ->
            withJSVal val $ \rval ->
                sendAsyncCommand $ SetPropertyByName rthis rname rval
{-# INLINE setPropertyByName #-}

setPropertyAtIndex :: Int -> JSVal -> Object -> JSM ()
setPropertyAtIndex index val this =
    withObject this $ \rthis ->
        withJSVal val $ \rval ->
            sendAsyncCommand $ SetPropertyAtIndex rthis index rval
{-# INLINE setPropertyAtIndex #-}

stringToValue :: JSString -> JSM JSVal
stringToValue s = withJSString s $ sendLazyCommand . StringToValue
{-# INLINE stringToValue #-}

numberToValue :: Double -> JSM JSVal
numberToValue = sendLazyCommand . NumberToValue
{-# INLINE numberToValue #-}

jsonValueToValue :: Value -> JSM JSVal
jsonValueToValue = sendLazyCommand . JSONValueToValue
{-# INLINE jsonValueToValue #-}

getPropertyByName :: JSString -> Object -> JSM JSVal
getPropertyByName name this =
    withObject this $ \rthis ->
        withJSString name $ sendLazyCommand . GetPropertyByName rthis
{-# INLINE getPropertyByName #-}

getPropertyAtIndex :: Int -> Object -> JSM JSVal
getPropertyAtIndex index this =
    withObject this $ \rthis -> sendLazyCommand $ GetPropertyAtIndex rthis index
{-# INLINE getPropertyAtIndex #-}

callAsFunction :: Object -> Object -> [JSVal] -> JSM JSVal
callAsFunction f this args =
    withObject f $ \rfunction ->
        withObject this $ \rthis ->
            withJSVals args $ sendLazyCommand . CallAsFunction rfunction rthis
{-# INLINE callAsFunction #-}

callAsConstructor :: Object -> [JSVal] -> JSM JSVal
callAsConstructor f args =
    withObject f $ \rfunction ->
        withJSVals args $ sendLazyCommand . CallAsConstructor rfunction
{-# INLINE callAsConstructor #-}

newEmptyObject :: JSM Object
newEmptyObject = Object <$> sendLazyCommand NewEmptyObject
{-# INLINE newEmptyObject #-}

newAsyncCallback :: JSCallAsFunction -> JSM Object
newAsyncCallback f = do
    object <- Object <$> sendLazyCommand NewAsyncCallback
    add <- addCallback <$> askJSM
    liftIO $ add object f
    return object
{-# INLINE newAsyncCallback #-}

newSyncCallback :: JSCallAsFunction -> JSM Object
newSyncCallback f = do
    object <- Object <$> sendLazyCommand NewSyncCallback
    add <- addCallback <$> askJSM
    liftIO $ add object f
    return object
{-# INLINE newSyncCallback #-}

newArray :: [JSVal] -> JSM JSVal
newArray xs = withJSVals xs $ \xs' -> sendLazyCommand (NewArray xs')
{-# INLINE newArray #-}

evaluateScript :: JSString -> JSM JSVal
evaluateScript script = withJSString script $ sendLazyCommand . EvaluateScript
{-# INLINE evaluateScript #-}

deRefVal :: JSVal -> JSM Result
deRefVal value = withJSVal value $ sendCommand . DeRefVal
{-# INLINE deRefVal #-}

valueToBool :: JSVal -> JSM Bool
valueToBool v@(JSVal ref) = liftIO (readIORef ref) >>= \case
    0 -> return False -- null
    1 -> return False -- undefined
    2 -> return False -- false
    3 -> return True  -- true
    _ -> withJSVal v $ \rval -> do
        ~(ValueToBoolResult result) <- sendCommand (ValueToBool rval)
        return result
{-# INLINE valueToBool #-}

valueToNumber :: JSVal -> JSM Double
valueToNumber value =
    withJSVal value $ \rval -> do
        ~(ValueToNumberResult result) <- sendCommand (ValueToNumber rval)
        return result
{-# INLINE valueToNumber #-}

valueToString :: JSVal -> JSM JSString
valueToString value = withJSVal value $ \rval -> do
    ~(ValueToStringResult result) <- sendCommand (ValueToString rval)
    wrapJSString result
{-# INLINE valueToString #-}

valueToJSON :: JSVal -> JSM JSString
valueToJSON value = withJSVal value $ \rval -> do
    ~(ValueToJSONResult result) <- sendCommand (ValueToJSON rval)
    wrapJSString result
{-# INLINE valueToJSON #-}

valueToJSONValue :: JSVal -> JSM Value
valueToJSONValue value = withJSVal value $ \rval -> do
    ~(ValueToJSONValueResult result) <- sendCommand (ValueToJSONValue rval)
    return result
{-# INLINE valueToJSONValue #-}

isNull :: JSVal -> JSM Bool
isNull v@(JSVal ref) = liftIO (readIORef ref) >>= \case
    0 -> return True  -- null
    1 -> return False -- undefined
    2 -> return False -- false
    3 -> return False -- true
    _ -> withJSVal v $ \rval -> do
        ~(IsNullResult result) <- sendCommand $ IsNull rval
        return result
{-# INLINE isNull #-}

isUndefined :: JSVal -> JSM Bool
isUndefined v@(JSVal ref) = liftIO (readIORef ref) >>= \case
    0 -> return False -- null
    1 -> return True  -- undefined
    2 -> return False -- false
    3 -> return False -- true
    _ -> withJSVal v $ \rval -> do
        ~(IsUndefinedResult result) <- sendCommand $ IsUndefined rval
        return result
{-# INLINE isUndefined #-}

strictEqual :: JSVal -> JSVal -> JSM Bool
strictEqual a b =
    withJSVal a $ \aref ->
        withJSVal b $ \bref -> do
            ~(StrictEqualResult result) <- sendCommand $ StrictEqual aref bref
            return result
{-# INLINE strictEqual #-}

instanceOf :: JSVal -> Object -> JSM Bool
instanceOf value constructor =
    withJSVal value $ \rval ->
        withObject constructor $ \c' -> do
            ~(InstanceOfResult result) <- sendCommand $ InstanceOf rval c'
            return result
{-# INLINE instanceOf #-}

propertyNames :: Object -> JSM [JSString]
propertyNames this =
    withObject this $ \rthis -> do
        ~(PropertyNamesResult result) <- sendCommand $ PropertyNames rthis
        mapM wrapJSString result
{-# INLINE propertyNames #-}
