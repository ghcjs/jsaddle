{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHCJS.Prim ( module GHCJS.Prim.Internal
                  , fromJSString
                  , toJSString
                  , isNull
                  , isUndefined
                  ) where

import           GHCJS.Prim.Internal
--import           Data.Int (Int64)
--import           Data.Typeable (Typeable)
--import           Unsafe.Coerce (unsafeCoerce)

--import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Text as T (unpack, pack)
import           Data.JSString.Text (textFromJSVal)

--import           GHC.Prim
--import qualified GHC.Exception as Ex
--import qualified GHC.Exts as Exts

import Language.Javascript.JSaddle.Types (JSVal(..), JSString(..), GHCJSPure(..), ghcjsPureMap)
import qualified Language.Javascript.JSaddle.Native.Internal as N
       (stringToValue, isNull, isUndefined)
{- | Low-level conversion utilities for packages that cannot
     depend on ghcjs-base
 -}

fromJSString :: JSVal -> GHCJSPure String
fromJSString = ghcjsPureMap T.unpack . textFromJSVal
{-# INLINE fromJSString #-}

toJSString :: String -> GHCJSPure JSVal
toJSString s = GHCJSPure $ N.stringToValue (JSString $ T.pack s)
{-# INLINE toJSString #-}

--fromJSArray :: JSVal -> IO [JSVal]
--fromJSArray = unsafeCoerce . js_fromJSArray
--{-# INLINE fromJSArray #-}
--
--toJSArray :: [JSVal] -> IO JSVal
--toJSArray = js_toJSArray . unsafeCoerce . seqList
--{-# INLINE toJSArray #-}
--
--{- | returns zero if the JSVal does not contain a number
-- -}
--fromJSInt :: JSVal -> Int
--fromJSInt = js_fromJSInt
--{-# INLINE fromJSInt #-}
--
--toJSInt :: Int -> JSVal
--toJSInt = js_toJSInt
--{-# INLINE toJSInt #-}
--

isNull :: JSVal -> GHCJSPure Bool
isNull = GHCJSPure . N.isNull
{-# INLINE isNull #-}

isUndefined :: JSVal -> GHCJSPure Bool
isUndefined = GHCJSPure . N.isUndefined
{-# INLINE isUndefined #-}

--getProp :: JSVal -> String -> IO JSVal
--getProp o p = js_getProp o (unsafeCoerce $ seqList p)
--{-# INLINE getProp #-}
--
--getProp' :: JSVal -> JSVal -> IO JSVal
--getProp' o p = js_getProp' o p
--{-# INLINE getProp' #-}
--
---- reduce the spine and all list elements to whnf
--seqList :: [a] -> [a]
--seqList xs = go xs `seq` xs
--  where go (x:xs) = x `seq` go xs
--        go []     = ()
--
--seqListSpine :: [a] -> [a]
--seqListSpine xs = go xs `seq` xs
--  where go (x:xs) = go xs
--        go []     = ()
