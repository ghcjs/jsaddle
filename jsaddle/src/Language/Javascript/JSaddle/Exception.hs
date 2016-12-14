{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Exception
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Exception (
    JSException(..)
--  , rethrow
) where

import qualified Control.Exception as E (Exception)
#ifdef ghcjs_HOST_OS
import GHCJS.Prim (JSVal)
#else
import GHCJS.Prim.Internal (JSVal)
#endif
import Data.Typeable (Typeable)

newtype JSException = JSException JSVal deriving (Typeable)

-- | JavaScript exception
-- >>>
instance Show JSException where show _ = "JSException"
instance E.Exception JSException

---- | Catch JavaScript exceptions and rethrow Haskell ones
--rethrow :: (MutableJSArray -> JSM a) -> JSM a
--rethrow f = f `catchval` \e ->
--    liftIO . E.throwIO $ JSException e

