module Main (
    main
) where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc",
    "src/Language/Javascript/JSaddle/Arguments.hs",
    "src/Language/Javascript/JSaddle/Classes.hs",
    "src/Language/Javascript/JSaddle/Evaluate.hs",
    "src/Language/Javascript/JSaddle/Exception.hs",
    "src/Language/Javascript/JSaddle/Monad.hs",
    "src/Language/Javascript/JSaddle/Native.hs",
    "src/Language/Javascript/JSaddle/Object.hs",
    "src/Language/Javascript/JSaddle/Properties.hs",
    "src/Language/Javascript/JSaddle/String.hs",
    "src/Language/Javascript/JSaddle/Test.hs",
    "src/Language/Javascript/JSaddle/Types.hs",
    "src/Language/Javascript/JSaddle/Value.hs" ]

