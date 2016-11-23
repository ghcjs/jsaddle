{-# LANGUAGE OverloadedStrings #-}
module JavaScript.Array.Internal
    ( create
    , fromListIO
    , toListIO
    , read
    , push
    ) where

import Prelude hiding(read)
import Control.Monad (void)
import Language.Javascript.JSaddle.Types (JSM, JSVal, SomeJSArray(..), MutableJSArray, Object(..), JSString(..))
import Language.Javascript.JSaddle.Native.Internal
       (withObject, withJSVal, withJSString, withJSVals)
import Language.Javascript.JSaddle.Run
       (Command(..), Result(..), AsyncCommand(..), sendCommand, sendLazyCommand)

create :: JSM MutableJSArray
create = SomeJSArray <$> sendLazyCommand (NewArray [])
{-# INLINE create #-}

fromListIO :: [JSVal] -> JSM (SomeJSArray m)
fromListIO xs = withJSVals xs $ \xs' -> SomeJSArray <$> sendLazyCommand (NewArray xs')
{-# INLINE fromListIO #-}

toListIO :: SomeJSArray m -> JSM [JSVal]
toListIO (SomeJSArray x) =
    withObject (Object x) $ \this -> do
        l <- withJSString (JSString "length") $ sendLazyCommand . GetPropertyByName this
        withJSVal l $ \l' -> do
            ValueToNumberResult len <- sendCommand (ValueToNumber l')
            mapM (sendLazyCommand . GetPropertyAtIndex this) [0..round len - 1]
{-# INLINE toListIO #-}

read :: Int -> SomeJSArray m -> JSM JSVal
read n (SomeJSArray x) =
    withObject (Object x) $ \this -> sendLazyCommand $ GetPropertyAtIndex this n
{-# INLINE read #-}

push :: JSVal -> MutableJSArray -> JSM ()
push e (SomeJSArray x) =
    void $ withJSVal e $ \e' ->
        withObject (Object x) $ \this -> do
            f <- withJSString (JSString "push") $ sendLazyCommand . GetPropertyByName this
            withObject (Object f) $ \f' -> sendLazyCommand $ CallAsFunction f' this [e']
{-# INLINE push #-}
