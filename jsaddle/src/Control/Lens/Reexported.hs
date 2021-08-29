{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Control.Lens.Reexported (
    (^.),
    to,
    IndexPreservingGetter,
) where

#ifdef LENS


import Control.Lens


#else


import Data.Functor.Contravariant
import Data.Functor.Const

type Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s
type IndexPreservingGetter s a = Getter s a

type Getting r s a = (a -> Const r a) -> s -> Const r s

(^.) :: s -> Getting a s a -> a
s ^. l = getConst (l Const s)
{-# INLINE (^.) #-}

to :: (s -> a) -> Getter s a
to k f = phantom . f . k
{-# INLINE to #-}


#endif
