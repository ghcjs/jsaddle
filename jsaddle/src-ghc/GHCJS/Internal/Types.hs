module GHCJS.Internal.Types ( IsJSVal(..)
                            , jsval
                            , MutabilityType(..)
                            , Mutable
                            , Immutable
                            , IsItMutable(..)
                            , Mutability
                            ) where

import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Native.Internal (stringToValue)

instance IsJSVal JSString where
  jsval_ a = GHCJSPure $ stringToValue a
  {-# INLINE jsval_ #-}

