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

  -- * Data.Text helpers
  , textFromJSString
  , textToJSString
  , strToText
  , textToStr
) where

import Data.Text (Text)
import Language.Javascript.JSaddle.Types (JSString(..))
#ifdef ghcjs_HOST_OS
import Data.JSString.Text (textFromJSString, textToJSString)
#endif

#ifndef ghcjs_HOST_OS
textFromJSString :: JSString -> Text
textFromJSString (JSString text) = text

textToJSString :: Text -> JSString
textToJSString = JSString
#endif

-- | Convert a JavaScript string to a Haskell 'Text'
strToText :: JSString -> Text
strToText = textFromJSString

-- | Convert a Haskell 'Text' to a JavaScript string
textToStr :: Text -> JSString
textToStr = textToJSString
