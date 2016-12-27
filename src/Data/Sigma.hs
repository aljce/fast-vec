{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Data.Sigma where

import Data.Kind

import Data.Nat (Nat,SNat)

newtype Sigma (b :: Nat -> Type) = Sigma (forall r. (forall n. SNat n -> b n -> r) -> r)
