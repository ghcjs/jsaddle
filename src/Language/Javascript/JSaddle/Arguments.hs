{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Arguments
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Arguments (
    MakeArgs(..)
) where

import Language.Javascript.JSaddle.Classes
       (ToJSVal(..))
import Language.Javascript.JSaddle.Monad (JSM)
import Language.Javascript.JSaddle.Types (JSVal)

-- | Anything that can be used to make a list of JavaScript value
--   references for use as function arguments
class MakeArgs this where
    makeArgs :: this -> JSM [JSVal]

instance MakeArgs arg => MakeArgs (JSM arg) where
    makeArgs arg = arg >>= makeArgs
    {-# INLINE makeArgs #-}

instance ToJSVal arg => MakeArgs [arg] where
    makeArgs = mapM toJSVal
    {-# INLINE makeArgs #-}

instance (ToJSVal arg1, ToJSVal arg2) => MakeArgs (arg1, arg2) where
    makeArgs (arg1, arg2) = do
        rarg1 <- toJSVal arg1
        rarg2 <- toJSVal arg2
        return [rarg1, rarg2]
    {-# INLINE makeArgs #-}

instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3) => MakeArgs (arg1, arg2, arg3) where
    makeArgs (arg1, arg2, arg3) = do
        rarg1 <- toJSVal arg1
        rarg2 <- toJSVal arg2
        rarg3 <- toJSVal arg3
        return [rarg1, rarg2, rarg3]
    {-# INLINE makeArgs #-}

instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3, ToJSVal arg4) => MakeArgs (arg1, arg2, arg3, arg4) where
    makeArgs (arg1, arg2, arg3, arg4) = do
        rarg1 <- toJSVal arg1
        rarg2 <- toJSVal arg2
        rarg3 <- toJSVal arg3
        rarg4 <- toJSVal arg4
        return [rarg1, rarg2, rarg3, rarg4]
    {-# INLINE makeArgs #-}

instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3, ToJSVal arg4, ToJSVal arg5) => MakeArgs (arg1, arg2, arg3, arg4, arg5) where
    makeArgs (arg1, arg2, arg3, arg4, arg5) = do
        rarg1 <- toJSVal arg1
        rarg2 <- toJSVal arg2
        rarg3 <- toJSVal arg3
        rarg4 <- toJSVal arg4
        rarg5 <- toJSVal arg5
        return [rarg1, rarg2, rarg3, rarg4, rarg5]
    {-# INLINE makeArgs #-}


instance (ToJSVal arg1, ToJSVal arg2, ToJSVal arg3, ToJSVal arg4, ToJSVal arg5, ToJSVal arg6) => MakeArgs (arg1, arg2, arg3, arg4, arg5, arg6) where
    makeArgs (arg1, arg2, arg3, arg4, arg5, arg6) = do
        rarg1 <- toJSVal arg1
        rarg2 <- toJSVal arg2
        rarg3 <- toJSVal arg3
        rarg4 <- toJSVal arg4
        rarg5 <- toJSVal arg5
        rarg6 <- toJSVal arg6
        return [rarg1, rarg2, rarg3, rarg4, rarg5, rarg6]
    {-# INLINE makeArgs #-}

