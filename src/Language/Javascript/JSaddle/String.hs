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
    JSString
  , MakeString(..)

  -- * Data.Text helpers
  , strToText
  , textToStr
) where

import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import Control.Monad.IO.Class (MonadIO(..))
import Language.Javascript.JSaddle.Types (JSString)
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import Data.JSString.Text (textFromJSString, textToJSString)
#else
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
       (jsstringcreatewithcharacters, jsstringgetcharactersptr,
        jsstringgetlength, jsstringcreatewithutf8cstring)
#endif
import qualified Data.Text.Foreign as T (fromPtr)
import Foreign (castPtr)
import Data.Text.Foreign (useAsPtr)
import Language.Javascript.JSaddle.Classes (MakeString(..))
import System.IO.Unsafe (unsafePerformIO)

-- | If we already have a JSStringRef we are fine
instance MakeString JSString where
    makeString = id
    {-# INLINE makeString #-}

instance MakeString Text where
    makeString = textToStr
    {-# INLINE makeString #-}

instance MakeString String where
    makeString = textToStr . T.pack
    {-# INLINE makeString #-}

-- | Convert a JavaScript string to a Haskell 'Text'
strToText :: MonadIO m => JSString -> m Text
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
strToText = return . textFromJSString
#else
strToText jsstring = liftIO $ do
    l <- jsstringgetlength jsstring
    p <- jsstringgetcharactersptr jsstring
    T.fromPtr (castPtr p) (fromIntegral l)
#endif
{-# INLINE strToText #-}

-- | Convert a Haskell 'Text' to a JavaScript string
textToStr :: Text -> JSString
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
textToStr = textToJSString
#else
textToStr text = unsafePerformIO $
    useAsPtr text $ \p l ->
        jsstringcreatewithcharacters (castPtr p) (fromIntegral l)
#endif
{-# INLINE textToStr #-}



