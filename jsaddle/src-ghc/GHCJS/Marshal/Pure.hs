{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHCJS.Marshal.Pure
  ( PFromJSVal(..)
  , PToJSVal(..)
  ) where

import           GHCJS.Marshal.Internal
import           Language.Javascript.JSaddle.Types (JSVal(..))
import           Language.Javascript.JSaddle.Foreign (jsFalse, jsTrue)

instance PFromJSVal JSVal where pFromJSVal = id
                                {-# INLINE pFromJSVal #-}
instance PFromJSVal ()    where pFromJSVal _ = ()
                                {-# INLINE pFromJSVal #-}

instance PToJSVal JSVal     where pToJSVal = id
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Bool      where pToJSVal True     = jsTrue
                                  pToJSVal False    = jsFalse
                                  {-# INLINE pToJSVal #-}
