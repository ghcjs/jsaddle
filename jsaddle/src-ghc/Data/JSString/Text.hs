{- | Conversion between 'Data.Text.Text' and 'Data.JSString.JSString'  -}

module Data.JSString.Text
    ( textToJSString
    , textFromJSString
    , lazyTextToJSString
    , lazyTextFromJSString
    , textFromJSVal
    , lazyTextFromJSVal
    ) where

import GHCJS.Prim.Internal (JSVal)

import Data.Coerce (coerce)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.JSString.Internal.Type

import Language.Javascript.JSaddle.Types (JSM, GHCJSPure(..))
import Language.Javascript.JSaddle.Native.Internal
       (valueToString)

textToJSString :: T.Text -> JSString
textToJSString = coerce
{-# INLINE textToJSString #-}

textFromJSString :: JSString -> T.Text
textFromJSString = coerce
{-# INLINE  textFromJSString #-}

lazyTextToJSString :: TL.Text -> JSString
lazyTextToJSString = coerce . TL.toStrict
{-# INLINE lazyTextToJSString #-}

lazyTextFromJSString :: JSString -> TL.Text
lazyTextFromJSString = TL.fromStrict . coerce
{-# INLINE lazyTextFromJSString #-}

-- | returns the empty Text if not a string
textFromJSVal :: JSVal -> GHCJSPure T.Text
textFromJSVal j = GHCJSPure $ textFromJSString <$> valToJSString j
{-# INLINE textFromJSVal #-}

-- | returns the empty Text if not a string
lazyTextFromJSVal :: JSVal -> GHCJSPure TL.Text
lazyTextFromJSVal j = GHCJSPure $ lazyTextFromJSString <$> valToJSString j
{-# INLINE lazyTextFromJSVal #-}

valToJSString :: JSVal -> JSM JSString
valToJSString = valueToString
{-# INLINE valToJSString #-}
