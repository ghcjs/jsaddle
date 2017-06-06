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
run :: Int -> IO () -> IO ()
run _port = id
#else
run :: Int -> JSM () -> IO ()
run port f =
    runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
        jsaddleOr defaultConnectionOptions (f >> syncPoint) jsaddleApp
#endif
