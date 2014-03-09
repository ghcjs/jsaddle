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
    MakeArgRefs(..)
) where

import Language.Javascript.JSaddle.Classes
       (MakeValueRef(..), MakeValueRef, MakeArgRefs(..), MakeArgRefs)
import Language.Javascript.JSaddle.Monad (JSM)

instance MakeArgRefs arg => MakeArgRefs (JSM arg) where
    makeArgRefs arg = arg >>= makeArgRefs

instance MakeValueRef arg => MakeArgRefs [arg] where
    makeArgRefs = mapM makeValueRef

instance (MakeValueRef arg1, MakeValueRef arg2) => MakeArgRefs (arg1, arg2) where
    makeArgRefs (arg1, arg2) = do
        rarg1 <- makeValueRef arg1
        rarg2 <- makeValueRef arg2
        return [rarg1, rarg2]

instance (MakeValueRef arg1, MakeValueRef arg2, MakeValueRef arg3) => MakeArgRefs (arg1, arg2, arg3) where
    makeArgRefs (arg1, arg2, arg3) = do
        rarg1 <- makeValueRef arg1
        rarg2 <- makeValueRef arg2
        rarg3 <- makeValueRef arg3
        return [rarg1, rarg2, rarg3]

instance (MakeValueRef arg1, MakeValueRef arg2, MakeValueRef arg3, MakeValueRef arg4) => MakeArgRefs (arg1, arg2, arg3, arg4) where
    makeArgRefs (arg1, arg2, arg3, arg4) = do
        rarg1 <- makeValueRef arg1
        rarg2 <- makeValueRef arg2
        rarg3 <- makeValueRef arg3
        rarg4 <- makeValueRef arg4
        return [rarg1, rarg2, rarg3, rarg4]

instance (MakeValueRef arg1, MakeValueRef arg2, MakeValueRef arg3, MakeValueRef arg4, MakeValueRef arg5) => MakeArgRefs (arg1, arg2, arg3, arg4, arg5) where
    makeArgRefs (arg1, arg2, arg3, arg4, arg5) = do
        rarg1 <- makeValueRef arg1
        rarg2 <- makeValueRef arg2
        rarg3 <- makeValueRef arg3
        rarg4 <- makeValueRef arg4
        rarg5 <- makeValueRef arg5
        return [rarg1, rarg2, rarg3, rarg4, rarg5]

