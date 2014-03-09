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
  , rethrow
) where

import qualified Control.Exception as E (throwIO, Exception)
import Language.Javascript.JSaddle.Types
       (JSValueRefRef, JSValueRef)
import Data.Typeable (Typeable)
import Language.Javascript.JSaddle.Monad (catchval, JSM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)

data JSException = JSException JSValueRef deriving (Show, Typeable)

instance E.Exception JSException

-- | Catch JavaScript exceptions and rethrow Haskell ones
rethrow :: (JSValueRefRef -> JSM a) -> JSM a
rethrow f = f `catchval` \e -> do
    liftIO . E.throwIO $ JSException e

