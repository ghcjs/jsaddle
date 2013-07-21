{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC.Exception
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSC.Exception (
    JSException(..)
  , rethrow
) where

import qualified Control.Exception as E (throwIO, Exception)
import Language.Javascript.JSC.Types
       (JSValueRefRef, JSValueRef)
import Data.Typeable (Typeable)
import Language.Javascript.JSC.Monad (catchval, JSC)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)

data JSException = JSException JSValueRef deriving (Show, Typeable)

instance E.Exception JSException

-- | Catch JavaScript exceptions and rethrow Haskell ones
rethrow :: (JSValueRefRef -> JSC a) -> JSC a
rethrow f = f `catchval` \e -> do
    liftIO . E.throwIO $ JSException e

