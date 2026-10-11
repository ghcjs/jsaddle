module Language.Javascript.JSaddle.WebSockets.BatchSender
    ( withBatchSender
    ) where

import Control.Concurrent.Async (link, withAsync)
import Control.Concurrent.STM
    ( atomically, newEmptyTMVarIO, putTMVar, takeTMVar, tryTakeTMVar )
import Control.Monad (forever, void)
import Language.Javascript.JSaddle.Types (Batch)

-- | Send batches without blocking the pump.
-- Keep at most one active batch and one pending batch.
-- Sender failure interrupts connection.
-- On exit, cancel the sender and wait for it to stop.
--
-- This should be called only from the serial JSaddle batch pump.
-- Publish the next batch only after acknowledgement of the previous batch.
-- Replace only WebSocket copies of batches already delivered through HTTP.
withBatchSender :: (Batch -> IO ()) -> ((Batch -> IO ()) -> IO a) -> IO a
withBatchSender send action = do
    pending <- newEmptyTMVarIO
    let publish batch = atomically $ do
            void $ tryTakeTMVar pending
            putTMVar pending batch
        sender = forever $ atomically (takeTMVar pending) >>= send
    withAsync sender $ \worker -> do
        link worker
        action publish
