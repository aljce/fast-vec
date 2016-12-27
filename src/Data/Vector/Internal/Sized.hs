{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Data.Vector.Internal.Sized where

import Data.Vector (Vector)
import qualified Data.Vector.Generic.Internal.Sized as G

import Data.Nat (Nat)

newtype Vec a (n :: Nat) = Vec { getVec :: G.Vec Vector a n }

instance Show a => Show (Vec a n) where
  showsPrec p (Vec (G.Vec v)) = showsPrec p v
