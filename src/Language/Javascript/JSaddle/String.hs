{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.String
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | JavaScript string conversion functions
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.String (
    JSStringRef
  , MakeStringRef(..)

  -- * Data.Text helpers
  , strToText
  , textToStr
) where

import Data.Text (Text)
import qualified Data.Text as T (pack)
import Control.Monad.IO.Class (MonadIO(..))
import Language.Javascript.JSaddle.Types (JSStringRef)
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import GHCJS.Foreign (fromJSString, toJSString)
#else
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
       (jsstringcreatewithcharacters, jsstringgetcharactersptr,
        jsstringgetlength)
#endif
import qualified Data.Text.Foreign as T (fromPtr)
import Foreign (castPtr)
import Data.Text.Foreign (useAsPtr)
import Language.Javascript.JSaddle.Classes (MakeStringRef(..))
import System.IO.Unsafe (unsafePerformIO)

-- | If we already have a JSStringRef we are fine
instance MakeStringRef JSStringRef where
    makeStringRef = id

instance MakeStringRef Text where
    makeStringRef = textToStr

instance MakeStringRef String where
    makeStringRef = textToStr . T.pack

-- | Convert a JavaScript string to a Haskell 'Text'
strToText :: MonadIO m => JSStringRef -> m Text
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
strToText = return . fromJSString
#else
strToText jsstring = liftIO $ do
    l <- jsstringgetlength jsstring
    p <- jsstringgetcharactersptr jsstring
    T.fromPtr (castPtr p) (fromIntegral l)
#endif

-- | Convert a Haskell 'Text' to a JavaScript string
textToStr :: Text -> JSStringRef
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
textToStr = toJSString
#else
textToStr text = unsafePerformIO $
    useAsPtr text $ \p l ->
        jsstringcreatewithcharacters (castPtr p) (fromIntegral l)
#endif



