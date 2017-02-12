module Language.Javascript.JSaddle.CLib
    ( run
    ) where

run :: IO () -> IO ()
run = id
{-# INLINE run #-}
