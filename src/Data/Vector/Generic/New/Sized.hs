{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-unticked-promoted-constructors #-}
module Data.Vector.Generic.New.Sized where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.New as N

import Data.Vector.Generic.Internal.Sized (New(..),MVec(..),strengthenMVecM)
import Control.Monad.ST (ST)
import Control.Monad.Primitive (PrimMonad(..))

import Data.Coerce (coerce)

create :: (forall s. ST s (MVec (G.Mutable v) s a n)) -> New v a n
{-# INLINE create #-}
create p = New (N.create (coerce p))

run :: New v a n -> ST s (MVec (G.Mutable v) s a n)
{-# INLINE run #-}
run (New p) = coerce (N.run p)

runPrim :: PrimMonad m => New v a n -> m (MVec (G.Mutable v) (PrimState m) a n)
{-# INLINE runPrim #-}
runPrim (New p) = strengthenMVecM (N.runPrim p)

apply :: (forall s. MVec (G.Mutable v) s a n -> MVec (G.Mutable v) s a n) -> New v a n -> New v a n
{-# INLINE apply #-}
apply f (New p) = New (N.apply (getMVec . f . MVec) p)

modify :: (forall s. MVec (G.Mutable v) s a n -> ST s ()) -> New v a n -> New v a n
{-# INLINE modify #-}
modify f (New p) = New (N.modify (f . MVec) p)

