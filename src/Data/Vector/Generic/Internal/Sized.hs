{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Data.Vector.Generic.Internal.Sized where

import Data.Nat (Nat)
import qualified Data.Vector.Generic.New as N (New)

import Unsafe.Coerce (unsafeCoerce)

newtype Vec v a (n :: Nat) = Vec { getVec :: v a }

newtype MVec v s a (n :: Nat) = MVec { getMVec :: v s a }

newtype New v a (n :: Nat) = New { getNew :: N.New v a }

strengthenVecM :: m (v a) -> m (Vec v a n)
strengthenVecM = unsafeCoerce

strengthenMVecM :: m (v s a) -> m (MVec v s a n)
{-# INLINE strengthenMVecM #-}
strengthenMVecM = unsafeCoerce

