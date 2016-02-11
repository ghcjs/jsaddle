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
  , ToJSString(..)

  -- * Data.Text helpers
  , strToText
  , textToStr
  , nullJSString
) where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO(..))
import Language.Javascript.JSaddle.Types (JSString)
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import Data.JSString.Text (textFromJSString, textToJSString)
import GHCJS.Marshal.Internal (PFromJSVal(..))
import GHCJS.Types (nullRef)
#else
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef
       (jsstringcreatewithcharacters, jsstringgetcharactersptr,
        jsstringgetlength)
import Language.Javascript.JSaddle.Native (makeNewJSString, withJSString)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (nullPtr)
import qualified Data.Text.Foreign as T (fromPtr)
import Foreign (castPtr)
import Data.Text.Foreign (useAsPtr)
#endif
import Language.Javascript.JSaddle.Classes (ToJSString(..))

-- | Convert a JavaScript string to a Haskell 'Text'
strToText :: MonadIO m => JSString -> m Text
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
strToText = return . textFromJSString
#else
strToText jsstring' = liftIO $ withJSString jsstring' $ \jsstring -> do
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
        jsstringcreatewithcharacters (castPtr p) (fromIntegral l) >>= makeNewJSString
#endif
{-# INLINE textToStr #-}

nullJSString :: JSString
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
nullJSString = pFromJSVal nullRef
#else
nullJSString = unsafePerformIO $ newForeignPtr_ nullPtr
#endif

