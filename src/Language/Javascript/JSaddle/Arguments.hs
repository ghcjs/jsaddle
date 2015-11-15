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
       (MakeVal(..), MakeArgs(..))
import Language.Javascript.JSaddle.Monad (JSM)

instance MakeArgs arg => MakeArgs (JSM arg) where
    makeArgs arg = arg >>= makeArgs
    {-# INLINE makeArgs #-}

instance MakeVal arg => MakeArgs [arg] where
    makeArgs = mapM makeVal
    {-# INLINE makeArgs #-}

instance (MakeVal arg1, MakeVal arg2) => MakeArgs (arg1, arg2) where
    makeArgs (arg1, arg2) = do
        rarg1 <- makeVal arg1
        rarg2 <- makeVal arg2
        return [rarg1, rarg2]
    {-# INLINE makeArgs #-}

instance (MakeVal arg1, MakeVal arg2, MakeVal arg3) => MakeArgs (arg1, arg2, arg3) where
    makeArgs (arg1, arg2, arg3) = do
        rarg1 <- makeVal arg1
        rarg2 <- makeVal arg2
        rarg3 <- makeVal arg3
        return [rarg1, rarg2, rarg3]
    {-# INLINE makeArgs #-}

instance (MakeVal arg1, MakeVal arg2, MakeVal arg3, MakeVal arg4) => MakeArgs (arg1, arg2, arg3, arg4) where
    makeArgs (arg1, arg2, arg3, arg4) = do
        rarg1 <- makeVal arg1
        rarg2 <- makeVal arg2
        rarg3 <- makeVal arg3
        rarg4 <- makeVal arg4
        return [rarg1, rarg2, rarg3, rarg4]
    {-# INLINE makeArgs #-}

instance (MakeVal arg1, MakeVal arg2, MakeVal arg3, MakeVal arg4, MakeVal arg5) => MakeArgs (arg1, arg2, arg3, arg4, arg5) where
    makeArgs (arg1, arg2, arg3, arg4, arg5) = do
        rarg1 <- makeVal arg1
        rarg2 <- makeVal arg2
        rarg3 <- makeVal arg3
        rarg4 <- makeVal arg4
        rarg5 <- makeVal arg5
        return [rarg1, rarg2, rarg3, rarg4, rarg5]
    {-# INLINE makeArgs #-}


instance (MakeVal arg1, MakeVal arg2, MakeVal arg3, MakeVal arg4, MakeVal arg5, MakeVal arg6) => MakeArgs (arg1, arg2, arg3, arg4, arg5, arg6) where
    makeArgs (arg1, arg2, arg3, arg4, arg5, arg6) = do
        rarg1 <- makeVal arg1
        rarg2 <- makeVal arg2
        rarg3 <- makeVal arg3
        rarg4 <- makeVal arg4
        rarg5 <- makeVal arg5
        rarg6 <- makeVal arg6
        return [rarg1, rarg2, rarg3, rarg4, rarg5, rarg6]
    {-# INLINE makeArgs #-}

