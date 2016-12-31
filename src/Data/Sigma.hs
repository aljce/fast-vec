{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Data.Sigma where

import Data.Kind

import Data.Nat (Nat,SNat)

newtype Sigma (b :: Nat -> Type) = Sigma (forall r. (forall n. SNat n -> b n -> r) -> r)

natVal :: (forall n. SNat n -> r) -> Sigma b -> r
natVal get (Sigma p) = p (\x _ -> get x)

withSigma :: Sigma b -> (forall n. SNat n -> b n -> r) -> r
withSigma (Sigma p) cb = p cb

uncurrySigma :: (forall n. SNat n -> b n -> r) -> Sigma b -> r
uncurrySigma cb (Sigma p) = p cb
