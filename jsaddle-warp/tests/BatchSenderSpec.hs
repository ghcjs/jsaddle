module Main (main) where

import Control.Concurrent.Async
    ( ExceptionInLinkedThread(..), cancel, withAsync )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar, tryTakeMVar )
import Control.Concurrent.STM
    ( atomically, newTQueueIO, readTQueue, writeTQueue )
import Control.Exception (Exception, finally, fromException, throwIO)
import Control.Monad (forM_)
import Language.Javascript.JSaddle.Types (Batch(..))
import Language.Javascript.JSaddle.WebSockets.BatchSender (withBatchSender)
import System.Timeout (timeout)
import Test.Hspec

main :: IO ()
main = hspec $ around_ within $ describe "batch sender" $ do
    it "advances acknowledged batches while the WebSocket blocks" $ do
        started <- newEmptyMVar
        release <- newEmptyMVar
        delivered <- newTQueueIO
        let send (Batch _ _ n) = do
                if n == 1
                    then putMVar started () >> takeMVar release
                    else return ()
                atomically $ writeTQueue delivered n
        withBatchSender send $ \publish -> do
            publish $ batch 1
            takeMVar started
            -- These batches have HTTP acknowledgements.
            -- Keep only the latest pending WebSocket copy.
            forM_ [2..1000] $ publish . batch
            putMVar release ()
            first <- atomically $ readTQueue delivered
            latest <- atomically $ readTQueue delivered
            (first, latest) `shouldBe` (1, 1000)

    it "interrupts the connection on sender failure" $ do
        blocked <- newEmptyMVar
        let send _ = throwIO SendFailure
        (withBatchSender send $ \publish -> do
            publish $ batch 1
            takeMVar blocked) `shouldThrow` linkedSendFailure

    it "stops the sender on normal connection exit" $ do
        started <- newEmptyMVar
        stopped <- newEmptyMVar
        blocked <- newEmptyMVar
        let send _ = (putMVar started () >> takeMVar blocked)
                `finally` putMVar stopped ()
        withBatchSender send $ \publish -> do
            publish $ batch 1
            takeMVar started
        tryTakeMVar stopped `shouldReturn` Just ()

    it "preserves connection errors and stops the sender" $ do
        started <- newEmptyMVar
        stopped <- newEmptyMVar
        blocked <- newEmptyMVar
        let send _ = (putMVar started () >> takeMVar blocked)
                `finally` putMVar stopped ()
        (withBatchSender send $ \publish -> do
            publish $ batch 1
            takeMVar started
            throwIO ConnectionFailure) `shouldThrow` (== ConnectionFailure)
        tryTakeMVar stopped `shouldReturn` Just ()

    it "stops the sender when the connection is cancelled" $ do
        started <- newEmptyMVar
        stopped <- newEmptyMVar
        blockedSend <- newEmptyMVar
        blockedConnection <- newEmptyMVar
        let send _ = (putMVar started () >> takeMVar blockedSend)
                `finally` putMVar stopped ()
        withAsync (withBatchSender send $ \publish -> do
            publish $ batch 1
            takeMVar blockedConnection) $ \connection -> do
                takeMVar started
                cancel connection
                tryTakeMVar stopped `shouldReturn` Just ()

batch :: Int -> Batch
batch = Batch [] False

data Failure = SendFailure | ConnectionFailure
    deriving (Eq, Show)

instance Exception Failure

linkedSendFailure :: ExceptionInLinkedThread -> Bool
linkedSendFailure (ExceptionInLinkedThread _ exception) =
    fromException exception == Just SendFailure

within :: IO () -> IO ()
within action = do
    result <- timeout 5000000 action
    case result of
        Nothing -> expectationFailure "batch sender test timed out"
        Just () -> return ()
