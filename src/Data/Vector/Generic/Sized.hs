{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Data.Vector.Generic.Sized where

import Prelude (Maybe(..),otherwise,Functor(..),
                Monad,Num(..),(.),id,Int,Eq(..),
                Bool,Ord,Ordering)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V

import Data.Nat.Internal
import Data.Nat

import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

newtype Vec (n :: Nat) v a = Vec { getVec :: v a }

{-# INLINE fins #-}
fins :: (Vector v Int, Vector v (Fin n)) => v Int -> v (Fin n)
fins = V.map Fin

{-# INLINE unFins #-}
unFins :: (Vector v (Fin n), Vector v Int) => v (Fin n) -> v Int
unFins = V.map (\(Fin x) -> x)

{-# INLINE length #-}
length :: (Vector v a) => Vec n v a -> SNat n
length (Vec v) = SNat (V.length v)

{-# INLINE null #-}
null :: (Vector v a) => Vec n v a -> Maybe (IsZero n)
null (Vec v)
  | V.null v  = Just (unsafeCoerce SIsZ)
  | otherwise = Nothing

infixr 9 !
{-# INLINE (!) #-}
(!) :: (Vector v a) => Vec n v a -> Fin n -> a
(Vec v) ! (Fin i) = v `V.unsafeIndex` i

{-# INLINE head #-}
head :: (Vector v a) => Vec ('S n) v a -> a
head (Vec v) = V.unsafeHead v

{-# INLINE last #-}
last :: (Vector v a) => Vec ('S n) v a -> a
last (Vec v) = V.unsafeLast v

indexM :: (Vector v a, Monad m) => Vec n v a -> Fin n -> m a
indexM (Vec v) (Fin i) = v `V.unsafeIndexM` i

headM :: (Vector v a, Monad m) => Vec ('S n) v a  -> m a
headM (Vec v) = V.unsafeHeadM v

lastM :: (Vector v a, Monad m) => Vec ('S n) v a -> m a
lastM (Vec v) = V.unsafeLastM v

slice :: (Vector v a) => SNat i -> Vec (i + n) v a -> Vec n v a
slice (SNat start) (Vec v) = Vec (V.unsafeSlice start (V.length v - start) v)

init :: (Vector v a) => Vec ('S n) v a -> Vec n v a
init (Vec v) = Vec (V.unsafeInit v)

tail :: (Vector v a) => Vec ('S s) v a -> Vec n v a
tail (Vec v) = Vec (V.unsafeTail v)

-- take :: (Vector v a, IsNat i) => Vec (i + n) v a -> Vec i v a
-- take (SNat amount) (Vec v) = Vec (V.unsafeTake amount v)

drop :: (Vector v a) => SNat i -> Vec (i + n) v a -> Vec n v a
drop (SNat amount) (Vec v) = Vec (V.unsafeDrop amount v)

empty :: (Vector v a) => Vec 'Z v a
empty = Vec V.empty

singleton :: (Vector v a) => a -> Vec ('S 'Z) v a
singleton x = Vec (V.singleton x)

replicate :: (Vector v a) => SNat n -> a -> Vec n v a
replicate (SNat amount) x = Vec (V.replicate amount x)

replicate' :: (Vector v a, IsNat n) => a -> Vec n v a
replicate' x = replicate witness x

replicateM :: (Vector v a, Monad m) => SNat n -> m a -> m (Vec n v a)
replicateM (SNat amount) x = fmap Vec (V.replicateM amount x)

replicateM' :: (Vector v a, Monad m, IsNat n) => m a -> m (Vec n v a)
replicateM' x = replicateM witness x

generate :: (Vector v a) => SNat n -> (Fin n -> a) -> Vec n v a
generate (SNat amount) f = Vec (V.generate amount (f . Fin))

generate' :: (Vector v a, IsNat n) => (Fin n -> a) -> Vec n v a
generate' f = generate witness f

generateM :: (Vector v a, Monad m) => SNat n -> (Fin n -> m a) -> m (Vec n v a)
generateM (SNat amount) f = fmap Vec (V.generateM amount (f . Fin))

generateM' :: (Vector v a, Monad m, IsNat n) => (Fin n -> m a) -> m (Vec n v a)
generateM' f = generateM witness f

allFin :: (Vector v (Fin n)) => SNat n -> Vec n v (Fin n)
allFin len = generate len id

allFin' :: (Vector v (Fin n), IsNat n) => Vec n v (Fin n)
allFin' = generate' id

iterate :: (Vector v a) => SNat n -> (a -> a) -> a -> Vec n v a
iterate (SNat amount) f x = Vec (V.iterateN amount f x)

iterate' :: (Vector v a, IsNat n) => (a -> a) -> a -> Vec n v a
iterate' f x = iterate witness f x

-- create

-- unfoldr

construct :: (Vector v a) => SNat n -> (forall m. Vec m v a -> a) -> Vec n v a
construct (SNat amount) f = Vec (V.constructN amount (f . Vec))

construct' :: (Vector v a, IsNat n) => (forall m. Vec m v a -> a) -> Vec n v a
construct' f = construct witness f

enumFromN :: (Vector v a, Num a) => a -> Fin n -> Vec n v a
enumFromN x (Fin len) = Vec (V.enumFromN x len)

enumFromStepN :: (Vector v a, Num a) => a -> a -> Fin n -> Vec n v a
enumFromStepN x y (Fin len) = Vec (V.enumFromStepN x y len)

-- enumFromTo but do we really want this slow function?

-- enumFromThenTo but do we really want this slow function?

cons :: (Vector v a) => a -> Vec n v a -> Vec ('S n) v a
cons x (Vec v) = Vec (V.cons x v)

snoc :: (Vector v a) => Vec n v a -> a -> Vec ('S n) v a
snoc (Vec v) x = Vec (V.snoc v x)

infixr 5 ++
(++) :: (Vector v a) => Vec n v a -> Vec m v a -> Vec (n + m) v a
(Vec v) ++ (Vec w) = Vec (v V.++ w)

-- concat :: Vec n v (Vec m v a) -> Vec (n * m) v a
-- concat (Vec vss) = concatMap id vss

force :: (Vector v a) => Vec n v a -> Vec n v a
force (Vec v) = Vec (V.force v)

(//) :: forall n v a. (Vector v a) => Vec n v a -> [(Fin n, a)] -> Vec n v a
(Vec v) // us = Vec (V.unsafeUpd v (coerce us :: [(Int, a)]))

{-# INLINE update #-}
update :: forall n v a m. (Vector v a, Vector v (Int, a)) =>
  Vec n v a -> Vec m v (Fin n, a) -> Vec n v a
update (Vec v) (Vec w) = Vec (V.unsafeUpdate v (unsafeCoerce w :: v (Int,a)))

update_ :: (Vector v a, Vector v Int, Vector v (Fin n)) =>
  Vec n v a -> Vec m v (Fin n) -> Vec m v a -> Vec n v a
update_ (Vec v) (Vec w) (Vec x) = Vec (V.unsafeUpdate_ v (unFins w) x)

accum :: forall n v a b. (Vector v a) => (a -> b -> a) -> Vec n v a -> [(Fin n, b)] -> Vec n v a
accum f (Vec v) us = Vec (V.unsafeAccum f v (coerce us :: [(Int, b)]))

accumulate :: forall n m v a b. (Vector v a,Vector v (Int, b)) =>
  (a -> b -> a) -> Vec n v a -> Vec m v (Fin n, b) -> Vec n v a
accumulate f (Vec v) (Vec w) = Vec (V.unsafeAccumulate f v (unsafeCoerce w :: v (Int,b)))

accumulate_ :: forall n m v a b. (Vector v a, Vector v Int, Vector v b, Vector v (Fin n)) =>
  (a -> b -> a) -> Vec n v a -> Vec m v (Fin n) -> Vec m v b -> Vec n v a
accumulate_ f (Vec v) (Vec w) (Vec x) = Vec (V.unsafeAccumulate_ f v (unFins w) x)

reverse :: (Vector v a) => Vec n v a -> Vec n v a
reverse (Vec v) = Vec (V.reverse v)

backpermute :: forall n m v a. (Vector v a, Vector v Int, Vector v (Fin n)) =>
  Vec n v a -> Vec m v (Fin n) -> Vec m v a
backpermute (Vec v) (Vec w) = Vec (V.unsafeBackpermute v (unFins w))

-- modify

indexed :: forall n v a. (Vector v a, Vector v (Int, a)) =>
  Vec n v a -> Vec n v (Fin n, a)
indexed (Vec v) = Vec (unsafeCoerce (V.indexed v) :: v (Fin n,a))

map :: (Vector v a, Vector v b) => (a -> b) -> Vec n v a -> Vec n v b
map f (Vec v) = Vec (V.map f v)

imap :: (Vector v a, Vector v b) => (Fin n -> a -> b) -> Vec n v a -> Vec n v b
imap f (Vec v) = Vec (V.imap (f . Fin) v)

concatMap :: (Vector v a, Vector v b) => (a -> Vec n v b) -> Vec m v a -> Vec (n * m) v b
concatMap f (Vec v) = Vec (V.concatMap (getVec . f) v)

mapM :: (Vector v a, Vector v b, Monad m) => (a -> m b) -> Vec n v a -> m (Vec n v b)
mapM f (Vec v) = fmap Vec (V.mapM f v)

imapM :: (Vector v a, Vector v b, Monad m) => (Fin n -> a -> m b) -> Vec n v a -> m (Vec n v b)
imapM f (Vec v) = fmap Vec (V.imapM (f . Fin) v)

mapM_ :: (Vector v a, Monad m) => (a -> m b) -> Vec n v a -> m ()
mapM_ f (Vec v) = V.mapM_ f v

imapM_ :: (Vector v a, Monad m) => (Fin n -> a -> m b) -> Vec n v a -> m ()
imapM_ f (Vec v) = V.imapM_ (f . Fin) v

forM :: (Vector v a, Vector v b, Monad m) => Vec n v a -> (a -> m b) -> m (Vec n v b)
forM (Vec v) f = fmap Vec (V.mapM f v)

forM_ :: (Vector v a, Monad m) => Vec n v a -> (a -> m b) -> m ()
forM_ (Vec v) f = V.mapM_ f v

zipWith :: (Vector v a, Vector v b, Vector v c) =>
  (a -> b -> c) -> Vec n v a -> Vec n v b -> Vec n v c
{-# INLINE zipWith #-}
zipWith f (Vec as) (Vec bs) = Vec (V.zipWith f as bs)

zipWith3 :: (Vector v a, Vector v b, Vector v c, Vector v d) =>
  (a -> b -> c -> d) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d
{-# INLINE zipWith3 #-}
zipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (V.zipWith3 f as bs cs)

zipWith4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e) =>
  (a -> b -> c -> d -> e) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e
{-# INLINE zipWith4 #-}
zipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (V.zipWith4 f as bs cs ds)

zipWith5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f) => (a -> b -> c -> d -> e -> f) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f
{-# INLINE zipWith5 #-}
zipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (V.zipWith5 f as bs cs ds es)

zipWith6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f, Vector v g) => (a -> b -> c -> d -> e -> f -> g) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f -> Vec n v g
{-# INLINE zipWith6 #-}
zipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (V.zipWith6 f as bs cs ds es fs)


izipWith :: (Vector v a, Vector v b, Vector v c) => (Fin n -> a -> b -> c) -> Vec n v a -> Vec n v b -> Vec n v c
{-# INLINE izipWith #-}
izipWith f (Vec as) (Vec bs) = Vec (V.izipWith (f . Fin) as bs)

izipWith3 :: (Vector v a, Vector v b, Vector v c, Vector v d) => (Fin n -> a -> b -> c -> d) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d
{-# INLINE izipWith3 #-}
izipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (V.izipWith3 (f . Fin) as bs cs)

izipWith4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e) => (Fin n -> a -> b -> c -> d -> e) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e
{-# INLINE izipWith4 #-}
izipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (V.izipWith4 (f . Fin) as bs cs ds)

izipWith5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f) => (Fin n -> a -> b -> c -> d -> e -> f) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f
{-# INLINE izipWith5 #-}
izipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (V.izipWith5 (f . Fin) as bs cs ds es)

izipWith6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f, Vector v g) => (Fin n -> a -> b -> c -> d -> e -> f -> g) -> Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f -> Vec n v g
{-# INLINE izipWith6 #-}
izipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (V.izipWith6 (f . Fin) as bs cs ds es fs)


zip :: (Vector v a, Vector v b, Vector v (a, b)) => Vec n v a -> Vec n v b -> Vec n v (a, b)
{-# INLINE zip #-}
zip (Vec as) (Vec bs) = Vec (V.zip as bs)

zip3 :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c)) => Vec n v a -> Vec n v b -> Vec n v c -> Vec n v (a, b, c)
{-# INLINE zip3 #-}
zip3 (Vec as) (Vec bs) (Vec cs) = Vec (V.zip3 as bs cs)

zip4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v (a, b, c, d)) => Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v (a, b, c, d)
{-# INLINE zip4 #-}
zip4 (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (V.zip4 as bs cs ds)

zip5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v (a, b, c, d, e)) => Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v (a, b, c, d, e)
{-# INLINE zip5 #-}
zip5 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (V.zip5 as bs cs ds es)

zip6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f, Vector v (a, b, c, d, e, f)) => Vec n v a -> Vec n v b -> Vec n v c -> Vec n v d -> Vec n v e -> Vec n v f -> Vec n v (a, b, c, d, e, f)
{-# INLINE zip6 #-}
zip6 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (V.zip6 as bs cs ds es fs)


unzip :: (Vector v a, Vector v b, Vector v (a, b)) => Vec n v (a, b) -> (Vec n v a, Vec n v b)
{-# INLINE unzip #-}
unzip (Vec vs) = (Vec as, Vec bs)
  where (as, bs) = V.unzip vs

unzip3 :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c)) => Vec n v (a, b, c) -> (Vec n v a, Vec n v b, Vec n v c)
{-# INLINE unzip3 #-}
unzip3 (Vec vs) = (Vec as, Vec bs, Vec cs)
  where (as, bs, cs) = V.unzip3 vs

unzip4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v (a, b, c, d)) => Vec n v (a, b, c, d) -> (Vec n v a, Vec n v b, Vec n v c, Vec n v d)
{-# INLINE unzip4 #-}
unzip4 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds)
  where (as, bs, cs, ds) = V.unzip4 vs

unzip5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v (a, b, c, d, e)) => Vec n v (a, b, c, d, e) -> (Vec n v a, Vec n v b, Vec n v c, Vec n v d, Vec n v e)
{-# INLINE unzip5 #-}
unzip5 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds, Vec es)
  where (as, bs, cs, ds, es) = V.unzip5 vs

unzip6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f, Vector v (a, b, c, d, e, f)) => Vec n v (a, b, c, d, e, f) -> (Vec n v a, Vec n v b, Vec n v c, Vec n v d, Vec n v e, Vec n v f)
{-# INLINE unzip6 #-}
unzip6 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds, Vec es, Vec fs)
  where (as, bs, cs, ds, es, fs) = V.unzip6 vs

-- filter
-- ifilter
-- filterM
-- takeWhile
-- dropWhile

-- partition
-- unstablePartition
-- span
-- break

infix 4 `elem`
elem :: (Vector v a, Eq a) => a -> Vec n v a -> Bool
elem x (Vec v) = V.elem x v

notElem :: (Vector v a, Eq a) => a -> Vec n v a -> Bool
notElem x (Vec v) = V.notElem x v

find :: (Vector v a) => (a -> Bool) -> Vec n v a -> Maybe a
find f (Vec v) = V.find f v

findIndex :: (Vector v a) => (a -> Bool) -> Vec n v a -> Maybe (Fin n)
findIndex f (Vec v) = coerce (V.findIndex f v)

data Sigma v a = Sigma (forall r. (forall n. SNat n -> Vec n v a -> r) -> r)

-- data Sigma v a where
  -- Sigma :: SNat n -> Vec n v a -> Sigma v a

nat :: Sigma v a -> Int
nat (Sigma f) = f (\x _ -> natToInt x)

findIndices :: forall n v a. (Vector v a, Vector v Int, Vector v (Fin n))
  => (a -> Bool) -> Vec n v a -> Sigma v (Fin n)
findIndices f (Vec v) = Sigma (\p -> p (SNat (V.length result)) (Vec result))
  where result = fins (V.findIndices f v) :: v (Fin n)

elemIndex :: (Vector v a, Eq a) => a -> Vec n v a -> Maybe (Fin n)
elemIndex x (Vec v) = coerce (V.elemIndex x v)

-- elemIndices :: (Vector v a, Vector v (Fin n), Eq a) => a -> Vec n v a -> Sigma v a

foldl :: (Vector v b) => (a -> b -> a) -> a -> Vec n v b -> a
foldl f x (Vec v) = V.foldl f x v

foldl1 :: (Vector v a) => (a -> a -> a) -> Vec ('S n) v a -> a
foldl1 f (Vec v) = V.foldl1 f v

foldl' :: (Vector v b) => (a -> b -> a) -> a -> Vec n v b -> a
foldl' f x (Vec v) = V.foldl' f x v

foldl1' :: (Vector v a) => (a -> a -> a) -> Vec ('S n) v a -> a
foldl1' f (Vec v) = V.foldl1' f v

foldlD :: (Vector v b) => (a m -> b -> a ('S m)) -> a 'Z -> (Vec n v b) -> a n
foldlD f x (Vec v) = V.foldl (unsafeCoerce f) (unsafeCoerce x) v

foldlD' :: (Vector v b) => (a m -> b -> a ('S m)) -> a 'Z -> (Vec n v b) -> a n
foldlD' f x (Vec v) = V.foldl' (unsafeCoerce f) (unsafeCoerce x) v

foldr :: (Vector v a) => (a -> b -> b) -> b -> Vec n v a -> b
foldr f x (Vec v) = V.foldr f x v

foldr1 :: (Vector v a) => (a -> a -> a) -> Vec ('S n) v a -> a
foldr1 f (Vec v) = V.foldr1 f v

foldr' :: (Vector v a) => (a -> b -> b) -> b -> Vec n v a -> b
foldr' f x (Vec v) = V.foldr' f x v

foldr1' :: (Vector v a) => (a -> a -> a) -> Vec ('S n) v a -> a
foldr1' f (Vec v) = V.foldr1' f v

foldrD :: (Vector v a) => (a -> b m -> b ('S n)) -> b 'Z -> Vec n v a -> b n
foldrD f x (Vec v) = V.foldr (unsafeCoerce f) (unsafeCoerce x) v

foldrD' :: (Vector v a) => (a -> b m -> b ('S n)) -> b 'Z -> Vec n v a -> b n
foldrD' f x (Vec v) = V.foldr' (unsafeCoerce f) (unsafeCoerce x) v

ifoldl :: (Vector v b) => (a -> Fin n -> b -> a) -> a -> Vec n v b -> a
ifoldl f x (Vec v) = V.ifoldl (coerce f) x v

ifoldl' :: (Vector v b) => (a -> Fin n -> b -> a) -> a -> Vec n v b -> a
ifoldl' f x (Vec v) = V.ifoldl' (coerce f) x v

ifoldlD :: (Vector v b) => (a m -> Fin n -> b -> a ('S m)) -> a 'Z -> (Vec n v b) -> a n
ifoldlD f x (Vec v) = V.ifoldl (unsafeCoerce f) (unsafeCoerce x) v

ifoldlD' :: (Vector v b) => (a m -> Fin n -> b -> a ('S m)) -> a 'Z -> (Vec n v b) -> a n
ifoldlD' f x (Vec v) = V.ifoldl' (unsafeCoerce f) (unsafeCoerce x) v

ifoldr :: (Vector v a) => (Fin n -> a -> b -> b) -> b -> Vec n v a -> b
ifoldr f x (Vec v) = V.ifoldr (coerce f) x v

ifoldr' :: (Vector v a) => (Fin n -> a -> b -> b) -> b -> Vec n v a -> b
ifoldr' f x (Vec v) = V.ifoldr' (coerce f) x v

ifoldrD :: (Vector v a) => (Fin n -> a -> b m -> b ('S n)) -> b 'Z -> Vec n v a -> b n
ifoldrD f x (Vec v) = V.ifoldr (unsafeCoerce f) (unsafeCoerce x) v

ifoldrD' :: (Vector v a) => (Fin n -> a -> b m -> b ('S n)) -> b 'Z -> Vec n v a -> b n
ifoldrD' f x (Vec v) = V.ifoldr' (unsafeCoerce f) (unsafeCoerce x) v

all :: (Vector v a) => (a -> Bool) -> Vec n v a -> Bool
all p (Vec v) = V.all p v

any :: (Vector v a) => (a -> Bool) -> Vec n v a -> Bool
any p (Vec v) = V.any p v

and :: (Vector v Bool) => Vec n v Bool -> Bool
and (Vec v) = V.and v

or :: (Vector v Bool) => Vec n v Bool -> Bool
or (Vec v) = V.or v

sum :: (Vector v a, Num a) => Vec n v a -> a
sum (Vec v) = V.sum v

product :: (Vector v a, Num a) => Vec n v a -> a
product (Vec v) = V.product v

maximum :: (Vector v a, Ord a) => Vec ('S n) v a -> a
maximum (Vec v) = V.maximum v

maximumBy :: (Vector v a) => (a -> a -> Ordering) -> Vec ('S n) v a -> a
maximumBy c (Vec v) = V.maximumBy c v

minimum :: (Vector v a, Ord a) => Vec ('S n) v a -> a
minimum (Vec v) = V.minimum v

minimumBy :: (Vector v a) => (a -> a -> Ordering) -> Vec ('S n) v a -> a
minimumBy c (Vec v) = V.minimumBy c v

minIndex :: (Vector v a, Ord a) => Vec ('S n) v a -> Fin ('S n)
minIndex (Vec v) = Fin (V.minIndex v)

minIndexBy :: (Vector v a) => (a -> a -> Ordering) -> Vec ('S n) v a -> Fin ('S n)
minIndexBy c (Vec v) = Fin (V.minIndexBy c v)

maxIndex :: (Vector v a, Ord a) => Vec ('S n) v a -> Fin ('S n)
maxIndex (Vec v) = Fin (V.maxIndex v)

maxIndexBy :: (Vector v a) => (a -> a -> Ordering) -> Vec ('S n) v a -> Fin ('S n)
maxIndexBy c (Vec v) = Fin (V.maxIndexBy c v)

