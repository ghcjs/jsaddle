{-# LANGUAGE DeriveDataTypeable #-}

module GHCJS.Concurrent ( OnBlocked(..)
                        , WouldBlockException(..)
                        ) where

import           GHCJS.Prim

import           Data.Data

{- |
     The runtime tries to run synchronous threads to completion. Sometimes it's
     not possible to continue running a thread, for example when the thread
     tries to take an empty 'MVar'. The runtime can then either throw a
     'WouldBlockException', aborting the blocking action, or continue the
     thread asynchronously.
 -}

data OnBlocked = ContinueAsync -- ^ continue the thread asynchronously if blocked
               | ThrowWouldBlock -- ^ throw 'WouldBlockException' if blocked
               deriving (Data, Typeable, Enum, Show, Eq, Ord)

