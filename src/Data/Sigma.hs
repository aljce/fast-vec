{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Data.Sigma where

import Data.Kind

import Data.Singletons (Sing,type (~>),type (@@))

newtype Sigma a (b :: a ~> Type) = Sigma (forall r. (forall x. Sing (x :: a) -> b @@ x -> r) -> r)

witness :: Sigma k b -> (forall x. Sing (x :: k) -> r) -> r
witness (Sigma p) proj1 = p (\x _ -> proj1 x)

withSigma :: Sigma a b -> (forall x. Sing (x :: a) -> b @@ x -> r) -> r
withSigma (Sigma p) cb = p cb

uncurrySigma :: (forall x. Sing (x :: a) -> b @@ x -> r) -> Sigma a b -> r
uncurrySigma cb (Sigma p) = p cb
