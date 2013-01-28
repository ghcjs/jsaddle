-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC.Monad
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | JSC monad keeps track of the JavaScript context
--
-----------------------------------------------------------------------------

module Language.Javascript.JSC.Monad (
  -- * Types
    JSC(..)
  , JSContextRef

  -- * Exception Handling
  , catchval
  , catch
) where

import Prelude hiding (catch)
import Control.Monad.Trans.Reader (runReaderT, ask, ReaderT(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSValueRefRef, JSValueRef, JSContextRef)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign (nullPtr, alloca)
import Foreign.Storable (Storable(..))
import qualified Control.Exception as E (Exception, catch)

-- | The @JSC@ monad keeps track of the JavaScript context.
--
-- Given a @JSC@ function and a 'JSContextRef' you can run the
-- function like this...
--
-- > runReaderT jscFunction javaScriptContext
--
-- For an example of how to set up WebKitGTK+ see tests/TestJSC.hs
type JSC = ReaderT JSContextRef IO

-- | Wrapped version of 'E.catch' that runs in a MonadIO that works
--   a bit better with 'JSC'
catch :: (MonadIO m, E.Exception e)
      => ReaderT r IO b
      -> (e -> ReaderT r IO b)
      -> ReaderT r m b
t `catch` c = do
    r <- ask
    liftIO (runReaderT t r `E.catch` \e -> runReaderT (c e) r)

-- | Handle JavaScriptCore functions that take a JSValueRefRef in order
--   to throw exceptions.
catchval :: (JSValueRefRef -> JSC a) -> (JSValueRef -> JSC a) -> JSC a
catchval f catcher = do
  gctxt <- ask
  liftIO . alloca $ \pexc -> flip runReaderT gctxt $ do
    liftIO $ poke pexc nullPtr
    result <- f pexc
    exc <- liftIO $ peek pexc
    if exc == nullPtr
        then return result
        else catcher exc


