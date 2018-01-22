{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.WebSockets.Compat
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | This module is necessary to harmonize various versions of the websockets
-- package; we can't do this CPP inline in
-- Language.Javascript.JSaddle.WebSockets because that module uses multi-line
-- strings, which are incompatible with CPP
--
-----------------------------------------------------------------------------
module Language.Javascript.JSaddle.WebSockets.Compat where

import Data.ByteString.Lazy (ByteString)
import qualified Network.WebSockets as WS (DataMessage(..))

getTextMessageByteString :: WS.DataMessage -> Maybe ByteString
getTextMessageByteString msg = case msg of
#if MIN_VERSION_websockets(0,11,0)
    (WS.Text t _) ->
#else
    (WS.Text t) ->
#endif
        Just t
    _ -> Nothing
