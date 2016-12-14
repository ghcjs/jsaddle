{-# LANGUAGE BangPatterns, MagicHash, RankNTypes #-}

module Data.JSString.Internal.Fusion.Common ( -- * Creation and elimination
                                              singleton
                                            , streamList
                                            , unstreamList
                                            , streamCString#

                                              -- * Basic interface
                                            , cons
                                            , snoc
                                            , append
                                            , head
                                            , uncons
                                            , last
                                            , tail
                                            , init
                                            , null
                                            , lengthI
                                            , compareLengthI
                                            , isSingleton

                                              -- * Transformations
                                            , map
                                            , intercalate
                                            , intersperse

                                              -- ** Case conversion
                                              -- $case
                                            , toCaseFold
                                            , toLower
                                            , toTitle
                                            , toUpper

                                              -- ** Justification
                                            , justifyLeftI

                                              -- * Folds
                                            , foldl
                                            , foldl'
                                            , foldl1
                                            , foldl1'
                                            , foldr
                                            , foldr1

                                              -- ** Special folds
                                            , concat
                                            , concatMap
                                            , any
                                            , all
                                            , maximum
                                            , minimum

                                              -- * Construction
                                              -- ** Scans
                                            , scanl

                                              -- ** Accumulating maps
                                              -- , mapAccumL

                                              -- ** Generation and unfolding
                                            , replicateCharI
                                            , replicateI
                                            , unfoldr
                                            , unfoldrNI

                                              -- * Substrings
                                              -- ** Breaking strings
                                            , take
                                            , drop
                                            , takeWhile
                                            , dropWhile

                                              -- * Predicates
                                            , isPrefixOf

                                              -- * Searching
                                            , elem
                                            , filter

                                              -- * Indexing
                                            , findBy
                                            , indexI
                                            , findIndexI
                                            , countCharI

                                              -- * Zipping and unzipping
                                            , zipWith
                                            ) where

import Data.Text.Internal.Fusion.Common
import Prelude ()
