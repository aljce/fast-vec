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
                Bool,Ord(..),Ordering)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as G

import Data.Sigma (Sigma(..))
import Data.Vector.Generic.Internal.Sized (Vec(..))

import Data.Nat.Internal
import Data.Nat

import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

fins :: (Vector v Int, Vector v (Fin n)) => v Int -> v (Fin n)
{-# INLINE fins #-}
fins = G.map Fin

unFins :: (Vector v (Fin n), Vector v Int) => v (Fin n) -> v Int
{-# INLINE unFins #-}
unFins = G.map (\(Fin x) -> x)

withLen :: (Vector v a) => v a -> Sigma (Vec v a)
withLen v = Sigma (\p -> p (SNat (G.length v)) (Vec v))

length :: (Vector v a) => Vec v a n -> SNat n
{-# INLINE length #-}
length (Vec v) = SNat (G.length v)

null :: (Vector v a) => Vec v a n -> Maybe (IsZero n)
{-# INLINE null #-}
null (Vec v)
  | G.null v  = Just (unsafeCoerce SIsZ)
  | otherwise = Nothing

infixr 9 !
(!) :: (Vector v a) => Vec v a n -> Fin n -> a
{-# INLINE (!) #-}
(Vec v) ! (Fin i) = G.unsafeIndex v i

head :: (Vector v a) => Vec v a ('S n) -> a
{-# INLINE head #-}
head (Vec v) = G.unsafeHead v

last :: (Vector v a) => Vec v a ('S n) -> a
{-# INLINE last #-}
last (Vec v) = G.unsafeLast v

indexM :: (Vector v a, Monad m) => Vec v a n -> Fin n -> m a
{-# INLINE indexM #-}
indexM (Vec v) (Fin i) = G.unsafeIndexM v i

headM :: (Vector v a, Monad m) => Vec v a ('S n) -> m a
{-# INLINE headM #-}
headM (Vec v) = G.unsafeHeadM v

lastM :: (Vector v a, Monad m) => Vec v a ('S n) -> m a
{-# INLINE lastM #-}
lastM (Vec v) = G.unsafeLastM v

slice :: (Vector v a) => SNat i -> Vec v a (i + n) -> Vec v a n
{-# INLINE slice #-}
slice (SNat start) (Vec v) = Vec (G.unsafeSlice start (G.length v - start) v)

init :: (Vector v a) => Vec v a ('S n) -> Vec v a n
{-# INLINE init #-}
init (Vec v) = Vec (G.unsafeInit v)

tail :: (Vector v a) => Vec v a ('S s) -> Vec v a n
{-# INLINE tail #-}
tail (Vec v) = Vec (G.unsafeTail v)

-- take :: (Vector v a, IsNat i) => Vec v a (i + n) -> Vec v a i
-- take (SNat amount) (Vec v) = Vec (G.unsafeTake amount v)

drop :: (Vector v a) => SNat i -> Vec v a (i + n) -> Vec v a n
{-# INLINE drop #-}
drop (SNat amount) (Vec v) = Vec (G.unsafeDrop amount v)

empty :: (Vector v a) => Vec v a 'Z
{-# INLINE empty #-}
empty = Vec G.empty

singleton :: (Vector v a) => a -> Vec v a ('S 'Z)
{-# INLINE singleton #-}
singleton x = Vec (G.singleton x)

replicate :: (Vector v a) => SNat n -> a -> Vec v a n
{-# INLINE replicate #-}
replicate (SNat amount) x = Vec (G.replicate amount x)

replicate' :: (Vector v a, IsNat n) => a -> Vec v a n
{-# INLINE replicate' #-}
replicate' x = replicate witness x

replicateM :: (Vector v a, Monad m) => SNat n -> m a -> m (Vec v a n)
{-# INLINE replicateM #-}
replicateM (SNat amount) x = fmap Vec (G.replicateM amount x)

replicateM' :: (Vector v a, Monad m, IsNat n) => m a -> m (Vec v a n)
{-# INLINE replicateM' #-}
replicateM' x = replicateM witness x

generate :: (Vector v a) => SNat n -> (Fin n -> a) -> Vec v a n
{-# INLINE generate  #-}
generate (SNat amount) f = Vec (G.generate amount (f . Fin))

generate' :: (Vector v a, IsNat n) => (Fin n -> a) -> Vec v a n
{-# INLINE generate' #-}
generate' f = generate witness f

generateM :: (Vector v a, Monad m) => SNat n -> (Fin n -> m a) -> m (Vec v a n)
{-# INLINE generateM #-}
generateM (SNat amount) f = fmap Vec (G.generateM amount (f . Fin))

generateM' :: (Vector v a, Monad m, IsNat n) => (Fin n -> m a) -> m (Vec v a n)
{-# INLINE generateM' #-}
generateM' f = generateM witness f

allFin :: (Vector v (Fin n)) => SNat n -> Vec v (Fin n) n
{-# INLINE allFin #-}
allFin len = generate len id

allFin' :: (Vector v (Fin n), IsNat n) => Vec v (Fin n) n
{-# INLINE allFin' #-}
allFin' = generate' id

iterate :: (Vector v a) => SNat n -> (a -> a) -> a -> Vec v a n
{-# INLINE iterate #-}
iterate (SNat amount) f x = Vec (G.iterateN amount f x)

iterate' :: (Vector v a, IsNat n) => (a -> a) -> a -> Vec v a n
{-# INLINE iterate' #-}
iterate' f x = iterate witness f x

-- create

-- unfoldr

construct :: (Vector v a) => SNat n -> (forall m. Vec v a m-> a) -> Vec v a n
{-# INLINE construct #-}
construct (SNat amount) f = Vec (G.constructN amount (f . Vec))

construct' :: (Vector v a, IsNat n) => (forall m. Vec v a m -> a) -> Vec v a n
{-# INLINE construct' #-}
construct' f = construct witness f

enumFromN :: (Vector v a, Num a) => a -> SNat n -> Vec v a n
{-# INLINE enumFromN #-}
enumFromN x (SNat len) = Vec (G.enumFromN x len)

enumFromN' :: (Vector v a, IsNat n, Num a) => a -> Vec v a n
{-# INLINE enumFromN' #-}
enumFromN' x = enumFromN x witness

enumFromStepN :: (Vector v a, Num a) => a -> a -> SNat n -> Vec v a n
{-# INLINE enumFromStepN #-}
enumFromStepN x y (SNat len) = Vec (G.enumFromStepN x y len)

enumFromStepN' :: (Vector v a, IsNat n, Num a) => a -> a -> Vec v a n
{-# INLINE enumFromStepN' #-}
enumFromStepN' x y = enumFromStepN x y witness

-- enumFromTo but do we really want this slow function?

-- enumFromThenTo but do we really want this slow function?

cons :: (Vector v a) => a -> Vec v a n -> Vec v a ('S n)
{-# INLINE cons #-}
cons x (Vec v) = Vec (G.cons x v)

snoc :: (Vector v a) => Vec v a n -> a -> Vec v a ('S n)
{-# INLINE snoc #-}
snoc (Vec v) x = Vec (G.snoc v x)

infixr 5 ++
(++) :: (Vector v a) => Vec v a n -> Vec v a m -> Vec v a (n + m)
{-# INLINE (++) #-}
(Vec v) ++ (Vec w) = Vec (v G.++ w)

-- concat :: Vec n v (Vec m v a) -> Vec (n * m) v a
-- concat (Vec vss) = concatMap id vss

force :: (Vector v a) => Vec v a n -> Vec v a n
{-# INLINE force #-}
force (Vec v) = Vec (G.force v)

(//) :: forall n v a. (Vector v a) => Vec v a n -> [(Fin n, a)] -> Vec v a n
{-# INLINE (//) #-}
(Vec v) // us = Vec (G.unsafeUpd v (coerce us :: [(Int, a)]))

update :: forall n v a m. (Vector v a, Vector v (Int, a)) =>
  Vec v a n -> Vec v (Fin n, a) m -> Vec v a n
{-# INLINE update #-}
update (Vec v) (Vec w) = Vec (G.unsafeUpdate v (unsafeCoerce w :: v (Int,a)))

update_ :: (Vector v a, Vector v Int, Vector v (Fin n)) =>
  Vec v a n -> Vec v (Fin n) m -> Vec v a m -> Vec v a n
{-# INLINE update_ #-}
update_ (Vec v) (Vec w) (Vec x) = Vec (G.unsafeUpdate_ v (unFins w) x)

accum :: forall n v a b. (Vector v a) => (a -> b -> a) -> Vec v a n -> [(Fin n, b)] -> Vec v a n
{-# INLINE accum #-}
accum f (Vec v) us = Vec (G.unsafeAccum f v (coerce us :: [(Int, b)]))

accumulate :: forall n m v a b. (Vector v a,Vector v (Int, b)) =>
  (a -> b -> a) -> Vec v a n -> Vec v (Fin n, b) m -> Vec v a n
{-# INLINE accumulate #-}
accumulate f (Vec v) (Vec w) = Vec (G.unsafeAccumulate f v (unsafeCoerce w :: v (Int,b)))

accumulate_ :: forall n m v a b. (Vector v a, Vector v Int, Vector v b, Vector v (Fin n)) =>
  (a -> b -> a) -> Vec v a n -> Vec v (Fin n) m -> Vec v b m -> Vec v a n
{-# INLINE accumulate_ #-}
accumulate_ f (Vec v) (Vec w) (Vec x) = Vec (G.unsafeAccumulate_ f v (unFins w) x)

reverse :: (Vector v a) => Vec v a n -> Vec v a n
{-# INLINE reverse #-}
reverse (Vec v) = Vec (G.reverse v)

backpermute :: forall n m v a. (Vector v a, Vector v Int, Vector v (Fin n)) =>
  Vec v a n -> Vec v (Fin n) m -> Vec v a m
{-# INLINE backpermute #-}
backpermute (Vec v) (Vec w) = Vec (G.unsafeBackpermute v (unFins w))

-- modify

indexed :: forall n v a. (Vector v a, Vector v (Int, a)) =>
  Vec v a n -> Vec v (Fin n, a) n
{-# INLINE indexed #-}
indexed (Vec v) = Vec (unsafeCoerce (G.indexed v) :: v (Fin n,a))

map :: (Vector v a, Vector v b) => (a -> b) -> Vec v a n -> Vec v b n
{-# INLINE map #-}
map f (Vec v) = Vec (G.map f v)

imap :: (Vector v a, Vector v b) => (Fin n -> a -> b) -> Vec v a n -> Vec v b n
{-# INLINE imap #-}
imap f (Vec v) = Vec (G.imap (f . Fin) v)

concatMap :: (Vector v a, Vector v b) => (a -> Vec v b n) -> Vec v a m -> Vec v b (n * m)
{-# INLINE concatMap #-}
concatMap f (Vec v) = Vec (G.concatMap (getVec . f) v)

mapM :: (Vector v a, Vector v b, Monad m) => (a -> m b) -> Vec v a n -> m (Vec v b n)
{-# INLINE mapM #-}
mapM f (Vec v) = fmap Vec (G.mapM f v)

imapM :: (Vector v a, Vector v b, Monad m) => (Fin n -> a -> m b) -> Vec v a n -> m (Vec v b n)
{-# INLINE imapM #-}
imapM f (Vec v) = fmap Vec (G.imapM (f . Fin) v)

mapM_ :: (Vector v a, Monad m) => (a -> m b) -> Vec v a n -> m ()
{-# INLINE mapM_ #-}
mapM_ f (Vec v) = G.mapM_ f v

imapM_ :: (Vector v a, Monad m) => (Fin n -> a -> m b) -> Vec v a n -> m ()
{-# INLINE imapM_ #-}
imapM_ f (Vec v) = G.imapM_ (f . Fin) v

forM :: (Vector v a, Vector v b, Monad m) => Vec v a n -> (a -> m b) -> m (Vec v b n)
{-# INLINE forM #-}
forM (Vec v) f = fmap Vec (G.mapM f v)

forM_ :: (Vector v a, Monad m) => Vec v a n -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ (Vec v) f = G.mapM_ f v

zipWith :: (Vector v a, Vector v b, Vector v c) =>
  (a -> b -> c) -> Vec v a n -> Vec v b n -> Vec v c n
{-# INLINE zipWith #-}
zipWith f (Vec as) (Vec bs) = Vec (G.zipWith f as bs)

zipWith3 :: (Vector v a, Vector v b, Vector v c, Vector v d) =>
  (a -> b -> c -> d) -> Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n
{-# INLINE zipWith3 #-}
zipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.zipWith3 f as bs cs)

zipWith4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e) =>
  (a -> b -> c -> d -> e) -> Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n -> Vec v e n
{-# INLINE zipWith4 #-}
zipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zipWith4 f as bs cs ds)

zipWith5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f) =>
  (a -> b -> c -> d -> e -> f) ->
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n -> Vec v e n -> Vec v f n
{-# INLINE zipWith5 #-}
zipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zipWith5 f as bs cs ds es)

zipWith6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f, Vector v g) =>
  (a -> b -> c -> d -> e -> f -> g) ->
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n -> Vec v e n -> Vec v f n -> Vec v g n
{-# INLINE zipWith6 #-}
zipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zipWith6 f as bs cs ds es fs)

izipWith :: (Vector v a, Vector v b, Vector v c) =>
  (Fin n -> a -> b -> c) -> Vec v a n -> Vec v b n -> Vec v c n
{-# INLINE izipWith #-}
izipWith f (Vec as) (Vec bs) = Vec (G.izipWith (f . Fin) as bs)

izipWith3 :: (Vector v a, Vector v b, Vector v c, Vector v d) =>
  (Fin n -> a -> b -> c -> d) ->
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n
{-# INLINE izipWith3 #-}
izipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.izipWith3 (f . Fin) as bs cs)

izipWith4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e) =>
  (Fin n -> a -> b -> c -> d -> e) ->
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n -> Vec v e n
{-# INLINE izipWith4 #-}
izipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.izipWith4 (f . Fin) as bs cs ds)

izipWith5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f) =>
  (Fin n -> a -> b -> c -> d -> e -> f) ->
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n -> Vec v e n -> Vec v f n
{-# INLINE izipWith5 #-}
izipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.izipWith5 (f . Fin) as bs cs ds es)

izipWith6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f, Vector v g) =>
  (Fin n -> a -> b -> c -> d -> e -> f -> g) ->
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n -> Vec v e n -> Vec v f n -> Vec v g n
{-# INLINE izipWith6 #-}
izipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.izipWith6 (f . Fin) as bs cs ds es fs)

zip :: (Vector v a, Vector v b, Vector v (a, b)) =>
  Vec v a n -> Vec v b n -> Vec v (a, b) n
{-# INLINE zip #-}
zip (Vec as) (Vec bs) = Vec (G.zip as bs)

zip3 :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c)) =>
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v (a, b, c) n
{-# INLINE zip3 #-}
zip3 (Vec as) (Vec bs) (Vec cs) = Vec (G.zip3 as bs cs)

zip4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v (a, b, c, d)) =>
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n -> Vec v (a, b, c, d) n
{-# INLINE zip4 #-}
zip4 (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zip4 as bs cs ds)

zip5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v (a, b, c, d, e)) =>
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n -> Vec v e n -> Vec v (a, b, c, d, e) n
{-# INLINE zip5 #-}
zip5 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zip5 as bs cs ds es)

zip6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f, Vector v (a, b, c, d, e, f)) =>
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n -> Vec v e n -> Vec v f n -> Vec v (a, b, c, d, e, f) n
{-# INLINE zip6 #-}
zip6 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zip6 as bs cs ds es fs)

unzip :: (Vector v a, Vector v b, Vector v (a, b)) =>
  Vec v (a, b) n -> (Vec v a n, Vec v b n)
{-# INLINE unzip #-}
unzip (Vec vs) = (Vec as, Vec bs)
  where (as, bs) = G.unzip vs

unzip3 :: (Vector v a, Vector v b, Vector v c, Vector v (a, b, c)) =>
  Vec v (a, b, c) n -> (Vec v a n, Vec v b n, Vec v c n)
{-# INLINE unzip3 #-}
unzip3 (Vec vs) = (Vec as, Vec bs, Vec cs)
  where (as, bs, cs) = G.unzip3 vs

unzip4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v (a, b, c, d)) =>
  Vec v (a, b, c, d) n -> (Vec v a n, Vec v b n, Vec v c n, Vec v d n)
{-# INLINE unzip4 #-}
unzip4 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds)
  where (as, bs, cs, ds) = G.unzip4 vs

unzip5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v (a, b, c, d, e)) =>
  Vec v (a, b, c, d, e) n -> (Vec v a n, Vec v b n, Vec v c n, Vec v d n, Vec v e n)
{-# INLINE unzip5 #-}
unzip5 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds, Vec es)
  where (as, bs, cs, ds, es) = G.unzip5 vs

unzip6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f, Vector v (a, b, c, d, e, f)) =>
  Vec v (a, b, c, d, e, f) n -> (Vec v a n, Vec v b n, Vec v c n, Vec v d n, Vec v e n, Vec v f n)
{-# INLINE unzip6 #-}
unzip6 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds, Vec es, Vec fs)
  where (as, bs, cs, ds, es, fs) = G.unzip6 vs

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
elem :: (Vector v a, Eq a) => a -> Vec v a n -> Bool
{-# INLINE elem #-}
elem x (Vec v) = G.elem x v

notElem :: (Vector v a, Eq a) => a -> Vec v a n -> Bool
{-# INLINE notElem #-}
notElem x (Vec v) = G.notElem x v

find :: (Vector v a) => (a -> Bool) -> Vec v a n -> Maybe a
{-# INLINE find #-}
find f (Vec v) = G.find f v

findIndex :: (Vector v a) => (a -> Bool) -> Vec v a n -> Maybe (Fin n)
{-# INLINE findIndex #-}
findIndex f (Vec v) = coerce (G.findIndex f v)

findIndices :: forall n v a. (Vector v a, Vector v Int, Vector v (Fin n))
  => (a -> Bool) -> Vec v a n -> Sigma (Vec v (Fin n))
{-# INLINE findIndices #-}
findIndices f (Vec v) = withLen (fins (G.findIndices f v))

elemIndex :: (Vector v a, Eq a) => a -> Vec v a n -> Maybe (Fin n)
{-# INLINE elemIndex #-}
elemIndex x (Vec v) = coerce (G.elemIndex x v)

elemIndices :: (Vector v a, Vector v Int, Vector v (Fin n), Eq a) => a -> Vec v a n -> Sigma (Vec v (Fin n))
elemIndices x (Vec v) = withLen (fins (G.elemIndices x v))

foldl :: (Vector v b) => (a -> b -> a) -> a -> Vec v b n -> a
{-# INLINE foldl #-}
foldl f x (Vec v) = G.foldl f x v

foldl1 :: (Vector v a) => (a -> a -> a) -> Vec v a ('S n) -> a
{-# INLINE foldl1 #-}
foldl1 f (Vec v) = G.foldl1 f v

foldl' :: (Vector v b) => (a -> b -> a) -> a -> Vec v b n -> a
{-# INLINE foldl' #-}
foldl' f x (Vec v) = G.foldl' f x v

foldl1' :: (Vector v a) => (a -> a -> a) -> Vec v a ('S n) -> a
{-# INLINE foldl1' #-}
foldl1' f (Vec v) = G.foldl1' f v

foldlD :: (Vector v b) => (a m -> b -> a ('S m)) -> a 'Z -> Vec v b n -> a n
{-# INLINE foldlD #-}
foldlD f x (Vec v) = G.foldl (unsafeCoerce f) (unsafeCoerce x) v

foldlD' :: (Vector v b) => (a m -> b -> a ('S m)) -> a 'Z -> Vec v b n -> a n
{-# INLINE foldlD' #-}
foldlD' f x (Vec v) = G.foldl' (unsafeCoerce f) (unsafeCoerce x) v

foldr :: (Vector v a) => (a -> b -> b) -> b -> Vec v a n -> b
{-# INLINE foldr #-}
foldr f x (Vec v) = G.foldr f x v

foldr1 :: (Vector v a) => (a -> a -> a) -> Vec v a ('S n) -> a
{-# INLINE foldr1 #-}
foldr1 f (Vec v) = G.foldr1 f v

foldr' :: (Vector v a) => (a -> b -> b) -> b -> Vec v a n -> b
{-# INLINE foldr' #-}
foldr' f x (Vec v) = G.foldr' f x v

foldr1' :: (Vector v a) => (a -> a -> a) -> Vec v a ('S n) -> a
{-# INLINE foldr1' #-}
foldr1' f (Vec v) = G.foldr1' f v

foldrD :: (Vector v a) => (a -> b m -> b ('S n)) -> b 'Z -> Vec v a n -> b n
{-# INLINE foldrD #-}
foldrD f x (Vec v) = G.foldr (unsafeCoerce f) (unsafeCoerce x) v

foldrD' :: (Vector v a) => (a -> b m -> b ('S n)) -> b 'Z -> Vec v a n -> b n
{-# INLINE foldrD' #-}
foldrD' f x (Vec v) = G.foldr' (unsafeCoerce f) (unsafeCoerce x) v

ifoldl :: (Vector v b) => (a -> Fin n -> b -> a) -> a -> Vec v b n -> a
{-# INLINE ifoldl #-}
ifoldl f x (Vec v) = G.ifoldl (coerce f) x v

ifoldl' :: (Vector v b) => (a -> Fin n -> b -> a) -> a -> Vec v b n -> a
{-# INLINE ifoldl' #-}
ifoldl' f x (Vec v) = G.ifoldl' (coerce f) x v

ifoldlD :: (Vector v b) => (a m -> Fin n -> b -> a ('S m)) -> a 'Z -> Vec v b n -> a n
{-# INLINE ifoldlD #-}
ifoldlD f x (Vec v) = G.ifoldl (unsafeCoerce f) (unsafeCoerce x) v

ifoldlD' :: (Vector v b) => (a m -> Fin n -> b -> a ('S m)) -> a 'Z -> Vec v b n -> a n
{-# INLINE ifoldlD' #-}
ifoldlD' f x (Vec v) = G.ifoldl' (unsafeCoerce f) (unsafeCoerce x) v

ifoldr :: (Vector v a) => (Fin n -> a -> b -> b) -> b -> Vec v a n -> b
{-# INLINE ifoldr #-}
ifoldr f x (Vec v) = G.ifoldr (coerce f) x v

ifoldr' :: (Vector v a) => (Fin n -> a -> b -> b) -> b -> Vec v a n -> b
{-# INLINE ifoldr' #-}
ifoldr' f x (Vec v) = G.ifoldr' (coerce f) x v

ifoldrD :: (Vector v a) => (Fin n -> a -> b m -> b ('S n)) -> b 'Z -> Vec v a n -> b n
{-# INLINE ifoldrD #-}
ifoldrD f x (Vec v) = G.ifoldr (unsafeCoerce f) (unsafeCoerce x) v

ifoldrD' :: (Vector v a) => (Fin n -> a -> b m -> b ('S n)) -> b 'Z -> Vec v a n -> b n
{-# INLINE ifoldrD' #-}
ifoldrD' f x (Vec v) = G.ifoldr' (unsafeCoerce f) (unsafeCoerce x) v

all :: (Vector v a) => (a -> Bool) -> Vec v a n -> Bool
{-# INLINE all #-}
all p (Vec v) = G.all p v

any :: (Vector v a) => (a -> Bool) -> Vec v a n -> Bool
{-# INLINE any #-}
any p (Vec v) = G.any p v

and :: (Vector v Bool) => Vec v Bool n -> Bool
{-# INLINE and #-}
and (Vec v) = G.and v

or :: (Vector v Bool) => Vec v Bool n -> Bool
{-# INLINE or #-}
or (Vec v) = G.or v

sum :: (Vector v a, Num a) => Vec v a n -> a
{-# INLINE sum #-}
sum (Vec v) = G.sum v

product :: (Vector v a, Num a) => Vec v a n -> a
{-# INLINE product #-}
product (Vec v) = G.product v

maximum :: (Vector v a, Ord a) => Vec v a ('S n) -> a
{-# INLINE maximum #-}
maximum (Vec v) = G.maximum v

maximumBy :: (Vector v a) => (a -> a -> Ordering) -> Vec v a ('S n) -> a
{-# INLINE maximumBy #-}
maximumBy c (Vec v) = G.maximumBy c v

minimum :: (Vector v a, Ord a) => Vec v a ('S n) -> a
{-# INLINE minimum #-}
minimum (Vec v) = G.minimum v

minimumBy :: (Vector v a) => (a -> a -> Ordering) -> Vec v a ('S n) -> a
{-# INLINE minimumBy #-}
minimumBy c (Vec v) = G.minimumBy c v

minIndex :: (Vector v a, Ord a) => Vec v a ('S n) -> Fin ('S n)
{-# INLINE minIndex #-}
minIndex (Vec v) = Fin (G.minIndex v)

minIndexBy :: (Vector v a) => (a -> a -> Ordering) -> Vec v a ('S n) -> Fin ('S n)
{-# INLINE minIndexBy #-}
minIndexBy c (Vec v) = Fin (G.minIndexBy c v)

maxIndex :: (Vector v a, Ord a) => Vec v a ('S n) -> Fin ('S n)
{-# INLINE maxIndex #-}
maxIndex (Vec v) = Fin (G.maxIndex v)

maxIndexBy :: (Vector v a) => (a -> a -> Ordering) -> Vec v a ('S n) -> Fin ('S n)
{-# INLINE maxIndexBy #-}
maxIndexBy c (Vec v) = Fin (G.maxIndexBy c v)

toList :: (Vector v a) => Vec v a n -> [a]
toList (Vec v) = G.toList v

-- fromList :: (Vector v a) => [a] -> Sigma v a
-- fromList list = Sigma (\p -> p (SNat (G.length result)) result)
--   where result = G.fromList list

fromListN :: (Vector v a) => SNat n -> [a] -> Maybe (Vec v a n)
fromListN (SNat len) list = fmap (Vec . G.fromList) (parseList 0 list)
  where parseList amount []
          | len <= amount = Just []
          | otherwise     = Nothing
        parseList amount (x:xs)
          | len <= amount = Just []
          | otherwise     = fmap (x:) (parseList (amount + 1) xs)
