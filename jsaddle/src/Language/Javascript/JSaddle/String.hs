{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
#ifdef ghcjs_HOST_OS
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
#endif
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
) where

import Data.Text (Text)
import Language.Javascript.JSaddle.Types (JSString(..))
#ifdef ghcjs_HOST_OS
import Data.JSString.Text (textFromJSString, textToJSString)
#endif
import Language.Javascript.JSaddle.Classes (ToJSString(..))

-- | Convert a JavaScript string to a Haskell 'Text'
strToText :: JSString -> Text
#ifdef ghcjs_HOST_OS
strToText = textFromJSString
#else
strToText (JSString text) = text
#endif

-- | Convert a Haskell 'Text' to a JavaScript string
textToStr :: Text -> JSString
#ifdef ghcjs_HOST_OS
textToStr = textToJSString
#else
textToStr = JSString
#endif

