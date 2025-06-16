{-# LANGUAGE CPP #-}
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

module Language.Javascript.JSaddle.Warp (
  -- * Running JSM over WebSockets
    run
#ifndef ghcjs_HOST_OS
  , module Language.Javascript.JSaddle.WebSockets
#endif
) where

import Data.ByteString.Lazy (ByteString)

#ifndef ghcjs_HOST_OS
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.WebSockets (defaultConnectionOptions)

import Language.Javascript.JSaddle.Types (JSM)
import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.WebSockets
#endif

-- | Run the given 'JSM' action as the main entry point.  Either directly
--   in GHCJS or as a Warp server on the given port on GHC.
#ifdef ghcjs_HOST_OS
run :: Maybe ByteString -> Int -> IO () -> IO ()
run _head _port = id
#else
run :: Maybe ByteString -> Int -> JSM () -> IO ()
run head_ port f =
    runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
        jsaddleOr head_ defaultConnectionOptions (f >> syncPoint) (jsaddleApp head_)
#endif
