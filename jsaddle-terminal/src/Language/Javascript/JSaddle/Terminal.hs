{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Run a jsaddle app from inside a terminal (e.g. a leksah / tmux pane).
--
-- 'run' handshakes with a watching IDE (leksah) over the process's OWN
-- stdin/stdout — see "Language.Javascript.JSaddle.Terminal.Protocol" — and,
-- when the IDE answers, tunnels jsaddle 'Batch'/'Results' JSON over that
-- channel; the UI renders in an IDE-hosted iframe.  No sockets are opened
-- here, so this works when the pane's machine is only reachable through
-- the IDE's existing ssh+tmux connection, and it works on every platform the
-- IDE runs on (POSIX termios / Windows console raw mode).
--
-- When nothing answers (not a tty, or the handshake times out) there is
-- nothing to tunnel to, so it just reports that and stops.  (An earlier
-- version fell back to serving the app over jsaddle-warp; that has been
-- removed.)
module Language.Javascript.JSaddle.Terminal
  ( run
  , runWith
  , Config(..)
  , defaultConfig
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Concurrent.STM
       (atomically, newTChanIO, readTChan, writeTChan, TChan)
import Control.Exception (bracket_, catch, SomeException)
import Control.Monad (forever, when)
import Data.Aeson (encode, decodeStrict')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Environment (lookupEnv)
import System.IO
       (stdin, stdout, stderr, hSetBinaryMode, hSetBuffering, hPutStrLn,
        BufferMode(..), hIsTerminalDevice, hFlush)
import System.Timeout (timeout)
#ifdef mingw32_HOST_OS
import Data.Bits ((.&.), (.|.), complement)
import Graphics.Win32.Misc (getStdHandle, sTD_INPUT_HANDLE, sTD_OUTPUT_HANDLE)
import System.Win32.Console
       (getConsoleMode, setConsoleMode, eNABLE_ECHO_INPUT, eNABLE_LINE_INPUT,
        eNABLE_PROCESSED_INPUT, eNABLE_VIRTUAL_TERMINAL_INPUT,
        eNABLE_VIRTUAL_TERMINAL_PROCESSING)
import System.Win32.Process (getCurrentProcessId)
#else
import System.Posix.IO (stdInput)
import System.Posix.Process (getProcessID)
import System.Posix.Terminal
       (getTerminalAttributes, setTerminalAttributes, withoutMode,
        withMinInput, withTime, TerminalAttributes, TerminalMode(..),
        TerminalState(Immediately))
#endif

import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.Run (runJavaScript)
import qualified Language.Javascript.JSaddle.Types as JS (Results)

import Language.Javascript.JSaddle.Terminal.Protocol

data Config = Config
  { helloIntervalUs :: Int   -- ^ µs between HELLO retries (default 500ms)
  , helloAttempts   :: Int   -- ^ retries before falling back (default 6 ≈ 3s)
  } deriving Show

defaultConfig :: Config
defaultConfig = Config
  { helloIntervalUs = 500000
  , helloAttempts   = 6
  }

-- | 'runWith' 'defaultConfig'.
run :: JSM () -> IO ()
run = runWith defaultConfig

-- | Why the tunnel session ended.
data Outcome = NoIde | Finished

-- | Debug tracing to a file, enabled by @JSADDLE_TERMINAL_DEBUG=<path>@
-- (stderr would be swallowed by the tunnel's raw tty).
debugLog :: String -> IO ()
debugLog msg = do
  mb <- lookupEnv "JSADDLE_TERMINAL_DEBUG"
  case mb of
    Nothing -> pure ()
    Just p  -> appendFile p (msg <> "\n") `catch` \(_ :: SomeException) -> pure ()

runWith :: Config -> JSM () -> IO ()
runWith cfg jsm = do
  ttyIn  <- hIsTerminalDevice stdin
  ttyOut <- hIsTerminalDevice stdout
  if not (ttyIn && ttyOut)
    then noIde "stdin/stdout is not a terminal"
    else do
      hSetBinaryMode stdin True
      hSetBuffering stdin NoBuffering
      hSetBinaryMode stdout True
      hSetBuffering stdout NoBuffering
      outcome <- withRawMode (tunnel cfg jsm)
      case outcome of
        NoIde    -> noIde "no IDE answered the handshake"
        Finished -> pure ()

-- | Nothing is driving the tunnel and there is no fallback server, so report
-- why and stop (the app is meant to be launched from inside leksah).
noIde :: String -> IO ()
noIde why = hPutStrLn stderr $
  "jsaddle-terminal: " <> why <> "; nothing to do (run this app from inside leksah)"

#ifdef mingw32_HOST_OS
-- | Windows console raw mode — the direct analog of the POSIX termios bracket.
-- Input: drop line editing, echo and processed-input (so Ctrl-C arrives as a
-- raw 0x03 rather than a signal, matching the reader), and enable VT input so
-- keys arrive as escape sequences.  Output: enable VT processing so the app's
-- ANSI escapes are interpreted.  Both modes are restored on exit.
withRawMode :: IO a -> IO a
withRawMode act = do
  hIn  <- getStdHandle sTD_INPUT_HANDLE
  hOut <- getStdHandle sTD_OUTPUT_HANDLE
  inSaved  <- getConsoleMode hIn
  outSaved <- getConsoleMode hOut
  let rawIn  = (inSaved .&. complement
                  (eNABLE_LINE_INPUT .|. eNABLE_ECHO_INPUT .|. eNABLE_PROCESSED_INPUT))
                 .|. eNABLE_VIRTUAL_TERMINAL_INPUT
      rawOut = outSaved .|. eNABLE_VIRTUAL_TERMINAL_PROCESSING
  bracket_
    (setConsoleMode hIn rawIn >> setConsoleMode hOut rawOut)
    (setConsoleMode hIn inSaved >> setConsoleMode hOut outSaved)
    act

-- | A per-run identifier for the HELLO payload (see 'handshake').
getRunPid :: IO Integer
getRunPid = toInteger <$> getCurrentProcessId
#else
-- | POSIX raw tty: no echo, no canonical mode, no signals, no flow control,
-- and — crucially — no IEXTEN (macOS @stty raw@ leaves it on, and its
-- VDISCARD / VLNEXT would eat 0x0f/0x16 from injected input), no CR mapping,
-- no output processing.  Restored on exit.
withRawMode :: IO a -> IO a
withRawMode act = do
  saved <- getTerminalAttributes stdInput
  bracket_
    (setTerminalAttributes stdInput (rawMode saved) Immediately)
    (setTerminalAttributes stdInput saved Immediately)
    act

rawMode :: TerminalAttributes -> TerminalAttributes
rawMode attrs =
  withTime (withMinInput cleared 1) 0
  where
    cleared = foldl withoutMode attrs
      [ EnableEcho, EchoErase, EchoKill, EchoLF
      , ProcessInput          -- ICANON
      , KeyboardInterrupts    -- ISIG
      , ExtendedFunctions     -- IEXTEN
      , StartStopOutput       -- IXON
      , MapCRtoLF             -- ICRNL
      , ProcessOutput         -- OPOST
      , InterruptOnBreak, CheckParity, StripHighBit
      ]

-- | A per-run identifier for the HELLO payload (see 'handshake').
getRunPid :: IO Integer
getRunPid = toInteger <$> getProcessID
#endif

-- | The tunnel session, inside the raw-termios bracket.
tunnel :: Config -> JSM () -> IO Outcome
tunnel cfg jsm = do
  wlock <- newMVar ()
  let writeFrame f = withMVar wlock $ \_ -> do
        BS.hPut stdout (encodeOsc f)
        hFlush stdout
  frames <- newTChanIO
  -- Reader: the only stdin consumer.  A raw 0x03 (Ctrl-C — ISIG is off) is
  -- outside any frame (frames are printable+RS+LF), so treat it as a close.
  _ <- forkIO $ readerLoop frames
  ok <- handshake cfg writeFrame frames
  debugLog ("handshake: " <> (if ok then "ACKed" else "timed out"))
  if not ok
    then pure NoIde
    else do
      (processResults, processSyncResults, start)
        <- runJavaScript
             (writeFrame . Frame Batch . LBS.toStrict . encode)
             jsm
      _ <- forkIO start
      let loop = atomically (readTChan frames) >>= \case
            Frame Results p -> case decodeStrict' p :: Maybe JS.Results of
              Just r  -> processResults r >> loop
              Nothing -> protoErr "undecodable RESULTS"
            Frame Sync p -> case decodeStrict' p :: Maybe JS.Results of
              -- processSyncResults blocks on the batch loop; never run it
              -- on the dispatch thread.
              Just r  -> do
                _ <- forkIO $
                  processSyncResults r
                    >>= writeFrame . Frame SyncReply . LBS.toStrict . encode
                loop
              Nothing -> protoErr "undecodable SYNC"
            Frame Close _ -> do
                debugLog "dispatch: CLOSE"
                writeFrame (Frame Bye "{}")
            f -> do
                debugLog ("dispatch: ignoring " <> show (frameType f))
                loop
          protoErr msg = do
            debugLog ("protocol error: " <> msg)
            hPutStrLn stderr ("jsaddle-terminal: protocol error: " <> msg)
            writeFrame (Frame Bye "{}")
      loop
      pure Finished

-- | Read stdin, scan RS frames into the channel; Ctrl-C injects Close.
readerLoop :: TChan Frame -> IO ()
readerLoop frames = do
  st <- newIORef emptyRsScan
  forever $ do
    chunk <- BS.hGetSome stdin 4096
    when (BS.null chunk) $ threadDelay 50000   -- EOF-ish; don't spin
    when (0x03 `BS.elem` chunk) $ do
      debugLog "reader: Ctrl-C"
      atomically $ writeTChan frames (Frame Close "{}")
    s <- readIORef st
    let (fs, s') = scanRs s chunk
    writeIORef st s'
    mapM_ (\f -> do debugLog ("reader: frame " <> show (frameType f))
                    atomically (writeTChan frames f)) fs

-- | Emit HELLO until an ACK (True), a CLOSE (False → fallback is still the
-- sane response) or the retry budget runs out (False).
--
-- The HELLO payload carries a per-run identifier (the pid): retries of the
-- SAME run are byte-identical, so the IDE can tell a retry still in flight
-- when its ACK lands (ignore/re-ACK) from a genuinely restarted app (tear
-- down the old iframe and rebuild) — without it, a late retry made the IDE
-- rebuild mid-handshake and drop the first batch.
handshake :: Config -> (Frame -> IO ()) -> TChan Frame -> IO Bool
handshake cfg writeFrame frames = do
    pid <- getRunPid
    let hello = Frame Hello $
          "{\"proto\":1,\"caps\":[\"sync\"],\"run\":" <> BSC.pack (show pid) <> "}"
        go 0 = pure False
        go n = do
          writeFrame hello
          r <- timeout (helloIntervalUs cfg) waitAck
          case r of
            Just b  -> pure b
            Nothing -> go (n - 1 :: Int)
        waitAck = atomically (readTChan frames) >>= \case
          Frame Ack _   -> pure True
          Frame Close _ -> pure False
          _             -> waitAck
    go (helloAttempts cfg)
