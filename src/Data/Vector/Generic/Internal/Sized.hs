{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Data.Vector.Generic.Internal.Sized where

import Data.Nat (Nat)

newtype Vec v a (n :: Nat) = Vec { getVec :: v a }
