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
import Language.Javascript.JSaddle.Types (JSString(..))
#ifdef ghcjs_HOST_OS
import Data.JSString.Text (textFromJSString, textToJSString)
import GHCJS.Marshal.Internal (PFromJSVal(..))
import GHCJS.Types (nullRef)
#else
import Language.Javascript.JSaddle.Native (wrapJSString, withJSString)
import Language.Javascript.JSaddle.WebSockets (Command(..), Result(..), sendCommand)
import Language.Javascript.JSaddle.Monad (JSM)
#endif
import Language.Javascript.JSaddle.Classes (ToJSString(..))

-- | Convert a JavaScript string to a Haskell 'Text'
strToText :: JSString -> JSM Text
#ifdef ghcjs_HOST_OS
strToText = return . textFromJSString
#else
strToText jsstring' = withJSString jsstring' $ \jsstring -> do
    JSStringToTextResult text <- sendCommand (JSStringToText jsstring)
    return text
#endif

-- | Convert a Haskell 'Text' to a JavaScript string
textToStr :: Text -> JSM JSString
#ifdef ghcjs_HOST_OS
textToStr = return . textToJSString
#else
textToStr text = do
    TextToJSStringResult str <- sendCommand (TextToJSString text)
    wrapJSString str
#endif

nullJSString :: JSString
#ifdef ghcjs_HOST_OS
nullJSString = pFromJSVal nullRef
#else
nullJSString = JSString 0
#endif

