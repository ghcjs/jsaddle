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
  , jsaddleOr
#endif
) where

import Language.Javascript.JSaddle.Types (JSM)
#ifndef ghcjs_HOST_OS
import Network.Wai.Handler.Warp
       (defaultSettings, setTimeout, setPort, runSettings)
import Network.WebSockets (defaultConnectionOptions)

import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.WebSockets (jsaddleOr, jsaddleApp)
#endif

-- | Run the given 'JSM' action as the main entry point.  Either directly
--   in GHCJS or as a Warp server on the given port on GHC.
run :: Int -> JSM () -> IO ()
#ifdef ghcjs_HOST_OS
run _port = id
#else
run port f =
    runSettings (setPort port (setTimeout 3600 defaultSettings)) $
        jsaddleOr defaultConnectionOptions (f >> syncPoint) jsaddleApp
#endif
