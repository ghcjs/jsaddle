-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Native
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Native (
    module Language.Javascript.JSaddle.Native.Internal
  , withToJSVal
) where

import GHCJS.Marshal.Internal (ToJSVal(..))
import Language.Javascript.JSaddle.Types
       (JSM(..), JSValueForSend(..))
import Language.Javascript.JSaddle.Native.Internal

withToJSVal :: ToJSVal val => val -> (JSValueForSend -> JSM a) -> JSM a
withToJSVal val f = do
    v <- toJSVal val
    withJSVal v f

