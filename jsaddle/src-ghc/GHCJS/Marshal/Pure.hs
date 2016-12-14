{-# OPTIONS_GHC -fno-warn-orphans #-}
module GHCJS.Marshal.Pure ( PFromJSVal(..)
                          , PToJSVal(..)
                          ) where

import           GHCJS.Types
import           GHCJS.Foreign.Internal (jsFalse, jsTrue)
import           GHCJS.Marshal.Internal

instance PFromJSVal JSVal where pFromJSVal = id
                                {-# INLINE pFromJSVal #-}
instance PFromJSVal ()    where pFromJSVal _ = ()
                                {-# INLINE pFromJSVal #-}

instance PToJSVal JSVal     where pToJSVal = id
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Bool      where pToJSVal True     = jsTrue
                                  pToJSVal False    = jsFalse
                                  {-# INLINE pToJSVal #-}
