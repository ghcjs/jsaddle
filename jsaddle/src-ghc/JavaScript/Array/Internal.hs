{-# LANGUAGE OverloadedStrings #-}
module JavaScript.Array.Internal
    ( SomeJSArray(..)
    , JSArray
    , MutableJSArray
    , STJSArray
    , create
    , fromList
    , fromListIO
    , toList
    , toListIO
    , index
    , read
    , push
    ) where

import Prelude hiding(read)
import Control.Monad (void)
import GHCJS.Types (JSVal)
import Data.JSString.Internal.Type (JSString(..))
import Language.Javascript.JSaddle.Types (JSM, SomeJSArray(..), JSArray, MutableJSArray, STJSArray, Object(..), GHCJSPure(..))
import Language.Javascript.JSaddle.Native.Internal
       (newArray, getPropertyByName, getPropertyAtIndex, callAsFunction, valueToNumber)

create :: JSM MutableJSArray
create = SomeJSArray <$> newArray []
{-# INLINE create #-}

fromList :: [JSVal] -> GHCJSPure (SomeJSArray m)
fromList = GHCJSPure . fromListIO
{-# INLINE fromList #-}

fromListIO :: [JSVal] -> JSM (SomeJSArray m)
fromListIO xs = SomeJSArray <$> newArray xs
{-# INLINE fromListIO #-}

toList :: SomeJSArray m -> GHCJSPure [JSVal]
toList = GHCJSPure . toListIO
{-# INLINE toList #-}

toListIO :: SomeJSArray m -> JSM [JSVal]
toListIO (SomeJSArray x) = do
    len <- getPropertyByName (JSString "length") (Object x) >>= valueToNumber
    mapM (`getPropertyAtIndex` Object x) [0..round len - 1]
{-# INLINE toListIO #-}

index :: Int -> SomeJSArray m -> GHCJSPure JSVal
index n = GHCJSPure . read n
{-# INLINE index #-}

read :: Int -> SomeJSArray m -> JSM JSVal
read n (SomeJSArray x) = getPropertyAtIndex n $ Object x
{-# INLINE read #-}

push :: JSVal -> MutableJSArray -> JSM ()
push e (SomeJSArray x) = void $ do
    f <- getPropertyByName (JSString "push") (Object x)
    void $ callAsFunction (Object f) (Object x) [e]
{-# INLINE push #-}
