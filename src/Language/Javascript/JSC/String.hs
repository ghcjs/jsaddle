{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC.String
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | JavaScript string conversion functions
--
-----------------------------------------------------------------------------

module Language.Javascript.JSC.String (
    JSStringRef
  , MakeStringRef(..)

  -- * Data.Text helpers
  , strToText
  , textToStr
) where

import Data.Text (Text)
import qualified Data.Text as T (pack)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
       (jsstringcreatewithcharacters, jsstringgetcharactersptr,
        jsstringgetlength)
import qualified Data.Text.Foreign as T (fromPtr)
import Foreign (castPtr)
import Data.Text.Foreign (useAsPtr)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase (JSStringRef)
import Language.Javascript.JSC.Classes (MakeStringRef(..))

-- | If we already have a JSStringRef we are fine
instance MakeStringRef JSStringRef where
    makeStringRef = return

instance MakeStringRef Text where
    makeStringRef = textToStr

instance MakeStringRef String where
    makeStringRef = textToStr . T.pack

-- | Convert a JavaScript string to a Haskell 'Text'
strToText :: MonadIO m => JSStringRef -> m Text
strToText jsstring = liftIO $ do
    l <- jsstringgetlength jsstring
    p <- jsstringgetcharactersptr jsstring
    T.fromPtr (castPtr p) (fromIntegral l)

-- | Convert a Haskell 'Text' to a JavaScript string
textToStr :: MonadIO m => Text -> m JSStringRef
textToStr text =
    liftIO $ useAsPtr text $ \p l ->
        jsstringcreatewithcharacters (castPtr p) (fromIntegral l)



