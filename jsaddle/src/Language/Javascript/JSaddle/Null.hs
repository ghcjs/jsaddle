{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Language.Javascript.JSaddle.Null (
    run
) where

import Language.Javascript.JSaddle.Types
       (BatchResults(..), JSM, JSStringReceived(..), Batch(..),
        Results(..), Result(..), Command(..))
import Control.Concurrent.Chan (readChan, writeChan, newChan)
import Language.Javascript.JSaddle.Run (runJavaScript)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Aeson (Value(..))
import Data.Maybe (mapMaybe)

-- | This is for performance testing JSaddle code that does not need to
-- to read anything back from the JavaScript context.
-- Anthing that does try to read will get JS null, 0 or "" back (depending
-- on how the value is read).
run :: JSM () -> IO ()
run f = do
    batches <- newChan
    (processResult, _processSyncResult, start) <- runJavaScript (writeChan batches) f
    _ <- forkIO $ forever $
        readChan batches >>= \case
            Batch commands _ batchNumber ->
                processResult $ BatchResults batchNumber . Success [] $ mapMaybe (\case
                        Left _ -> Nothing
                        Right command -> Just $
                            case command of
                                DeRefVal _ -> DeRefValResult 0 ""
                                ValueToBool _ -> ValueToBoolResult False
                                ValueToNumber _ -> ValueToNumberResult 0
                                ValueToString _ -> ValueToStringResult (JSStringReceived "")
                                ValueToJSON _ -> ValueToJSONResult (JSStringReceived "null")
                                ValueToJSONValue _ -> ValueToJSONValueResult Null
                                IsNull _ -> IsNullResult True
                                IsUndefined _ -> IsUndefinedResult False
                                StrictEqual _ _ -> StrictEqualResult False
                                InstanceOf _ _ -> InstanceOfResult False
                                PropertyNames _ -> PropertyNamesResult []
                                Sync -> SyncResult) commands
    start
