module Data.JSString.Internal.Search ( indices
                                     ) where

import Data.Coerce (coerce)

import Data.JSString.Internal.Type (JSString(..))

import qualified Data.Text.Internal.Search as T

indices :: JSString -> JSString -> [Int]
indices = coerce T.indices
