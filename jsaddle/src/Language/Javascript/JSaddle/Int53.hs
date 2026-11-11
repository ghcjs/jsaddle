-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Int53
-- License     :  MIT
--
-- | Integers that a JavaScript Number holds exactly ('Int53' and 'Word53',
-- with no 'Num' instance on purpose).  See "GHCJS.Int53": ghcjs-base
-- defines it for the JavaScript backend and jsaddle for native GHC, so the
-- types are the same whichever backend is used.
--
-----------------------------------------------------------------------------
module Language.Javascript.JSaddle.Int53 (
    module GHCJS.Int53
) where

import GHCJS.Int53
