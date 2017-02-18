{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror -Wno-unticked-promoted-constructors #-}
module Data.Vector.Generic.Sized (
  -- * Immutable vectors
  Vec,
  -- :* Accessors

  -- ** Length information
  length, null,

  -- ** Indexing
  (!), head, last,

  -- ** Monadic indexing
  indexM, headM, lastM,

  -- ** Extracting subvectors (slicing)
  slice, init, tail, take, drop, splitAt,

  -- :* Construction

  -- ** Initialisation
  empty, singleton, replicate, replicate',
  generate, generate', iterate, iterate',

  -- ** Monadic initialisation
  replicateM, replicateM', generateM, generateM', -- create,

  -- ** Unfolding
  unfoldr, unfoldrN,
  construct, construct', constructr, constructr',

  -- ** Enumeration
  enumFromN, enumFromN', enumFromStepN, enumFromStepN',

  -- ** Concatenation
  cons, snoc, (++), concat, diagonal,

  -- ** Restricting memory usage
  force,

  -- :* Modifying vectors

  -- ** Bulk updates
  (//), update, update_,

  -- ** Accumulations
  accum, accumulate, accumulate_,

  -- ** Permutations
  reverse, backpermute,

  -- ** Safe destructive updates
  modify,

  -- :* Elementwise operations

  -- ** Indexing
  indexed, allBound, allBound',

  -- ** Mapping
  map, imap, concatMap,

  -- ** Monadic mapping
  mapM, imapM, mapM_, imapM_, forM, forM_,

  -- ** Zipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,
  zip, zip3, zip4, zip5, zip6,

  -- ** Monadic zipping
  zipWithM, izipWithM, zipWithM_, izipWithM_,

  -- ** Unzipping
  unzip, unzip3, unzip4, unzip5, unzip6,

  -- :* Working with predicates

  -- ** Filtering
  filter, ifilter, filterM,
  takeWhile, dropWhile,

  -- ** Partitioning
  partition, unstablePartition, span, break,

  -- ** Searching
  elem, notElem, find, findIndex, findIndices, elemIndex, elemIndices,

  -- :* Folding
  foldl, foldl1, foldl', foldl1', foldlD, foldlD',
  foldr, foldr1, foldr', foldr1', foldrD, foldrD',
  ifoldl, ifoldl', ifoldlD, ifoldlD',
  ifoldr, ifoldr', ifoldrD, ifoldrD',

  -- ** Specialised folds
  all, any, and, or,
  sum, product,
  maximum, maximumBy, minimum, minimumBy,
  minIndex, minIndexBy, maxIndex, maxIndexBy,

  -- ** Monadic folds
  foldM, ifoldM, foldM', ifoldM',
  fold1M, fold1M', foldM_, ifoldM_,
  foldM'_, ifoldM'_, fold1M_, fold1M'_,
  foldMD, foldMD', ifoldMD, ifoldMD',

  -- ** Monadic sequencing
  sequence, sequence_,

  -- :* Prefix sums (scans)
  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl', scanl1, scanl1',
  prescanr, prescanr',
  postscanr, postscanr',
  scanr, scanr', scanr1, scanr1',

  -- :* Conversions

  -- ** Lists
  toList, fromList, fromListN, fromListN',

  -- ** Different vector types
  convert, toVector, fromVector, fromVectorN, fromVectorN',

  -- ** Mutable vectors
  freeze, thaw, copy, unsafeFreeze, unsafeThaw,

  -- :* Fusion support

  -- ** Conversion to/from Bundles
  stream, unstream, streamR, unstreamR,

  -- ** Recycling support
  new, clone,

  -- :* Utilities

  -- ** Comparisons
  eq, cmp,

  -- ** Show
  showsPrec
) where

import Prelude (Maybe(..),otherwise,Functor(..),   
                Monad,Num(..),(.),id,Int,Eq(..),
                Bool,Ord(..),Ordering,Show,ShowS)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Bundle as B

import Data.Sigma (Sigma(..))
import Data.Bifunctor (bimap)
import Data.Vector.Generic.Internal.Sized (Vec(..),MVec(..),New(..),strengthenVecM,strengthenMVecM)
import qualified Data.Vector.Generic.New.Sized as New (modify)
import Control.Monad.ST (ST)
import Control.Monad.Primitive (PrimMonad(..))

import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

import Data.Type.Equality ((:~:)(..))
import Data.Singletons.Prelude (SingI(..),PNum(..),POrd(..),type TyCon1)
import Data.Singletons.Decide (SDecide(..),Decision)
import Data.Nat.Internal (Sing(..),Bound(..))
import Data.Nat (type Nat(..),type SNat,zero)

fins :: v Int -> v (Bound n)
{-# INLINE fins #-}
fins = unsafeCoerce

unBounds :: v (Bound n) -> v Int
{-# INLINE unBounds #-}
unBounds = unsafeCoerce

length :: (Vector v a) => Vec v a n -> SNat n
{-# INLINE length #-}
length (Vec v) = SNat (G.length v)

null :: (Vector v a) => Vec v a n -> Decision (n :~: Z)
{-# INLINE null #-}
null v = length v %~ zero

infixr 9 !
(!) :: (Vector v a) => Vec v a n -> Bound n -> a
{-# INLINE (!) #-}
(Vec v) ! (Bound i) = G.unsafeIndex v i

head :: (Vector v a) => Vec v a (S n) -> a
{-# INLINE head #-}
head (Vec v) = G.unsafeHead v

last :: (Vector v a) => Vec v a (S n) -> a
{-# INLINE last #-}
last (Vec v) = G.unsafeLast v

indexM :: (Vector v a, Monad m) => Vec v a n -> Bound n -> m a
{-# INLINE indexM #-}
indexM (Vec v) (Bound i) = G.unsafeIndexM v i

headM :: (Vector v a, Monad m) => Vec v a (S n) -> m a
{-# INLINE headM #-}
headM (Vec v) = G.unsafeHeadM v

lastM :: (Vector v a, Monad m) => Vec v a (S n) -> m a
{-# INLINE lastM #-}
lastM (Vec v) = G.unsafeLastM v

slice :: (Vector v a) => SNat i -> Vec v a (i :+ n) -> Vec v a n
{-# INLINE slice #-}
slice (SNat start) (Vec v) = Vec (G.unsafeSlice start (G.length v - start) v)

init :: (Vector v a) => Vec v a (S n) -> Vec v a n
{-# INLINE init #-}
init (Vec v) = Vec (G.unsafeInit v)

tail :: (Vector v a) => Vec v a (S s) -> Vec v a n
{-# INLINE tail #-}
tail (Vec v) = Vec (G.unsafeTail v)

take :: (Vector v a) => SNat i -> Vec v a j -> Vec v a (Min i j)
{-# INLINE take #-}
take (SNat amount) (Vec v) = Vec (G.unsafeTake amount v)

drop :: (Vector v a) => SNat i -> Vec v a (i :+ n) -> Vec v a n
{-# INLINE drop #-}
drop (SNat amount) (Vec v) = Vec (G.unsafeDrop amount v)

splitAt :: (Vector v a) => SNat i -> Vec v a (i :+ n) -> (Vec v a i, Vec v a n)
{-# INLINE splitAt #-}
splitAt (SNat amount) (Vec v) = coerce (G.splitAt amount v)

empty :: (Vector v a) => Vec v a Z
{-# INLINE empty #-}
empty = Vec G.empty

singleton :: (Vector v a) => a -> Vec v a (S Z)
{-# INLINE singleton #-}
singleton x = Vec (G.singleton x)

replicate :: (Vector v a) => SNat n -> a -> Vec v a n
{-# INLINE replicate #-}
replicate (SNat amount) x = Vec (G.replicate amount x)

replicate' :: (Vector v a, SingI n) => a -> Vec v a n
{-# INLINE replicate' #-}
replicate' x = replicate sing x

replicateM :: (Vector v a, Monad m) => SNat n -> m a -> m (Vec v a n)
{-# INLINE replicateM #-}
replicateM (SNat amount) x = fmap Vec (G.replicateM amount x)

replicateM' :: (Vector v a, Monad m, SingI n) => m a -> m (Vec v a n)
{-# INLINE replicateM' #-}
replicateM' x = replicateM sing x

generate :: (Vector v a) => SNat n -> (Bound n -> a) -> Vec v a n
{-# INLINE generate  #-}
generate (SNat amount) f = Vec (G.generate amount (f . Bound))

generate' :: (Vector v a, SingI n) => (Bound n -> a) -> Vec v a n
{-# INLINE generate' #-}
generate' f = generate sing f

generateM :: (Vector v a, Monad m) => SNat n -> (Bound n -> m a) -> m (Vec v a n)
{-# INLINE generateM #-}
generateM (SNat amount) f = fmap Vec (G.generateM amount (f . Bound))

generateM' :: (Vector v a, Monad m, SingI n) => (Bound n -> m a) -> m (Vec v a n)
{-# INLINE generateM' #-}
generateM' f = generateM sing f

allBound :: (Vector v (Bound n)) => SNat n -> Vec v (Bound n) n
{-# INLINE allBound #-}
allBound len = generate len id

allBound' :: (Vector v (Bound n), SingI n) => Vec v (Bound n) n
{-# INLINE allBound' #-}
allBound' = generate' id

iterate :: (Vector v a) => SNat n -> (a -> a) -> a -> Vec v a n
{-# INLINE iterate #-}
iterate (SNat amount) f x = Vec (G.iterateN amount f x)

iterate' :: (Vector v a, SingI n) => (a -> a) -> a -> Vec v a n
{-# INLINE iterate' #-}
iterate' f x = iterate sing f x

-- create

unfoldr :: (Vector v a) => (b -> Maybe (a,b)) -> b -> Sigma Nat (TyCon1 (Vec v a))
{-# INLINE unfoldr #-}
unfoldr f x = fromVector (G.unfoldr f x)

unfoldrN :: (Vector v a) => SNat n -> (b -> Maybe (a,b)) -> b -> Maybe (Vec v a n)
{-# INLINE unfoldrN #-}
unfoldrN (SNat amount) f x
  | amount == G.length result = Just (Vec result)
  | otherwise = Nothing
  where result = G.unfoldrN amount f x

construct :: (Vector v a) => SNat n -> (forall m. Vec v a m -> a) -> Vec v a n
{-# INLINE construct #-}
construct (SNat amount) f = Vec (G.constructN amount (f . Vec))

construct' :: (Vector v a, SingI n) => (forall m. Vec v a m -> a) -> Vec v a n
{-# INLINE construct' #-}
construct' f = construct sing f

constructr :: (Vector v a) => SNat n -> (forall m. Vec v a m -> a) -> Vec v a n
{-# INLINE constructr #-}
constructr (SNat amount) f = Vec (G.constructrN amount (f . Vec))

constructr' :: (Vector v a, SingI n) => (forall m. Vec v a m -> a) -> Vec v a n
{-# INLINE constructr' #-}
constructr' f = constructr sing f

enumFromN :: (Vector v a, Num a) => a -> SNat n -> Vec v a n
{-# INLINE enumFromN #-}
enumFromN x (SNat len) = Vec (G.enumFromN x len)

enumFromN' :: (Vector v a, SingI n, Num a) => a -> Vec v a n
{-# INLINE enumFromN' #-}
enumFromN' x = enumFromN x sing

enumFromStepN :: (Vector v a, Num a) => a -> a -> SNat n -> Vec v a n
{-# INLINE enumFromStepN #-}
enumFromStepN x y (SNat len) = Vec (G.enumFromStepN x y len)

enumFromStepN' :: (Vector v a, SingI n, Num a) => a -> a -> Vec v a n
{-# INLINE enumFromStepN' #-}
enumFromStepN' x y = enumFromStepN x y sing

cons :: (Vector v a) => a -> Vec v a n -> Vec v a (S n)
{-# INLINE cons #-}
cons x (Vec v) = Vec (G.cons x v)

snoc :: (Vector v a) => Vec v a n -> a -> Vec v a (S n)
{-# INLINE snoc #-}
snoc (Vec v) x = Vec (G.snoc v x)

infixr 5 ++
(++) :: (Vector v a) => Vec v a n -> Vec v a m -> Vec v a (n :+ m)
{-# INLINE (++) #-}
(Vec v) ++ (Vec w) = Vec (v G.++ w)

concat :: (Vector v (Vec v a m), Vector v a) => Vec v (Vec v a m) n -> Vec v a (n :* m)
{-# INLINE concat #-}
concat (Vec vs) = Vec (G.foldr (\(Vec v) ac -> v G.++ ac) G.empty vs)

force :: (Vector v a) => Vec v a n -> Vec v a n
{-# INLINE force #-}
force (Vec v) = Vec (G.force v)

(//) :: forall n v a. (Vector v a) => Vec v a n -> [(Bound n, a)] -> Vec v a n
{-# INLINE (//) #-}
(Vec v) // us = Vec (G.unsafeUpd v (coerce us :: [(Int, a)]))

update :: forall n v a m. (Vector v a, Vector v (Int, a)) =>
  Vec v a n -> Vec v (Bound n, a) m -> Vec v a n
{-# INLINE update #-}
update (Vec v) (Vec w) = Vec (G.unsafeUpdate v (unsafeCoerce w :: v (Int,a)))

update_ :: (Vector v a, Vector v Int) => Vec v a n -> Vec v (Bound n) m -> Vec v a m -> Vec v a n
{-# INLINE update_ #-}
update_ (Vec v) (Vec w) (Vec x) = Vec (G.unsafeUpdate_ v (unBounds w) x)

accum :: forall n v a b. (Vector v a) => (a -> b -> a) -> Vec v a n -> [(Bound n, b)] -> Vec v a n
{-# INLINE accum #-}
accum f (Vec v) us = Vec (G.unsafeAccum f v (coerce us :: [(Int, b)]))

accumulate :: forall n m v a b. (Vector v a,Vector v (Int, b)) =>
  (a -> b -> a) -> Vec v a n -> Vec v (Bound n, b) m -> Vec v a n
{-# INLINE accumulate #-}
accumulate f (Vec v) (Vec w) = Vec (G.unsafeAccumulate f v (unsafeCoerce w :: v (Int,b)))

accumulate_ :: forall n m v a b. (Vector v a, Vector v Int, Vector v b) =>
  (a -> b -> a) -> Vec v a n -> Vec v (Bound n) m -> Vec v b m -> Vec v a n
{-# INLINE accumulate_ #-}
accumulate_ f (Vec v) (Vec w) (Vec x) = Vec (G.unsafeAccumulate_ f v (unBounds w) x)

reverse :: (Vector v a) => Vec v a n -> Vec v a n
{-# INLINE reverse #-}
reverse (Vec v) = Vec (G.reverse v)

backpermute :: forall n m v a. (Vector v a, Vector v Int) =>
  Vec v a n -> Vec v (Bound n) m -> Vec v a m
{-# INLINE backpermute #-}
backpermute (Vec v) (Vec w) = Vec (G.unsafeBackpermute v (unBounds w))

modify :: Vector v a => (forall s. MVec (G.Mutable v) s a n -> ST s ()) -> Vec v a n -> Vec v a n
{-# INLINE modify #-}
modify p = new . New.modify p . clone

indexed :: forall n v a. (Vector v a, Vector v (Int, a)) =>
  Vec v a n -> Vec v (Bound n, a) n
{-# INLINE indexed #-}
indexed (Vec v) = Vec (unsafeCoerce (G.indexed v) :: v (Bound n,a))

diagonal :: (Vector v (Vec v a n), Vector v (Bound n), Vector v a, SingI n) => Vec v (Vec v a n) n -> Vec v a n
{-# INLINE diagonal #-}
diagonal v = zipWith (!) v allBound'

map :: (Vector v a, Vector v b) => (a -> b) -> Vec v a n -> Vec v b n
{-# INLINE map #-}
map f (Vec v) = Vec (G.map f v)

imap :: (Vector v a, Vector v b) => (Bound n -> a -> b) -> Vec v a n -> Vec v b n
{-# INLINE imap #-}
imap f (Vec v) = Vec (G.imap (f . Bound) v)

concatMap :: (Vector v a, Vector v b) => (a -> Vec v b n) -> Vec v a m -> Vec v b (n :* m)
{-# INLINE concatMap #-}
concatMap f (Vec v) = Vec (G.concatMap (getVec . f) v)

mapM :: (Vector v a, Vector v b, Monad m) => (a -> m b) -> Vec v a n -> m (Vec v b n)
{-# INLINE mapM #-}
mapM f (Vec v) = fmap Vec (G.mapM f v)

imapM :: (Vector v a, Vector v b, Monad m) => (Bound n -> a -> m b) -> Vec v a n -> m (Vec v b n)
{-# INLINE imapM #-}
imapM f (Vec v) = fmap Vec (G.imapM (f . Bound) v)

mapM_ :: (Vector v a, Monad m) => (a -> m b) -> Vec v a n -> m ()
{-# INLINE mapM_ #-}
mapM_ f (Vec v) = G.mapM_ f v

imapM_ :: (Vector v a, Monad m) => (Bound n -> a -> m b) -> Vec v a n -> m ()
{-# INLINE imapM_ #-}
imapM_ f (Vec v) = G.imapM_ (f . Bound) v

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
  (Bound n -> a -> b -> c) -> Vec v a n -> Vec v b n -> Vec v c n
{-# INLINE izipWith #-}
izipWith f (Vec as) (Vec bs) = Vec (G.izipWith (f . Bound) as bs)

izipWith3 :: (Vector v a, Vector v b, Vector v c, Vector v d) =>
  (Bound n -> a -> b -> c -> d) ->
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n
{-# INLINE izipWith3 #-}
izipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.izipWith3 (f . Bound) as bs cs)

izipWith4 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e) =>
  (Bound n -> a -> b -> c -> d -> e) ->
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n -> Vec v e n
{-# INLINE izipWith4 #-}
izipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.izipWith4 (f . Bound) as bs cs ds)

izipWith5 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f) =>
  (Bound n -> a -> b -> c -> d -> e -> f) ->
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n -> Vec v e n -> Vec v f n
{-# INLINE izipWith5 #-}
izipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.izipWith5 (f . Bound) as bs cs ds es)

izipWith6 :: (Vector v a, Vector v b, Vector v c, Vector v d, Vector v e, Vector v f, Vector v g) =>
  (Bound n -> a -> b -> c -> d -> e -> f -> g) ->
  Vec v a n -> Vec v b n -> Vec v c n -> Vec v d n -> Vec v e n -> Vec v f n -> Vec v g n
{-# INLINE izipWith6 #-}
izipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.izipWith6 (f . Bound) as bs cs ds es fs)

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

zipWithM :: (Vector v a, Vector v b, Vector v c, Monad m) =>
  (a -> b -> m c) -> Vec v a n -> Vec v b n -> m (Vec v c n)
{-# INLINE zipWithM #-}
zipWithM f (Vec as) (Vec bs) = fmap Vec (G.zipWithM f as bs)

izipWithM :: (Vector v a, Vector v b, Vector v c, Monad m) =>
  (Bound n -> a -> b -> m c) -> Vec v a n -> Vec v b n -> m (Vec v c n)
{-# INLINE izipWithM #-}
izipWithM f (Vec as) (Vec bs) = fmap Vec (G.izipWithM (coerce f) as bs)

zipWithM_ :: (Vector v a, Vector v b, Monad m) =>
  (a -> b -> m c) -> Vec v a n -> Vec v b n -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f (Vec as) (Vec bs) = G.zipWithM_ f as bs

izipWithM_ :: (Vector v a, Vector v b, Monad m) =>
  (Bound n -> a -> b -> m c) -> Vec v a n -> Vec v b n -> m ()
{-# INLINE izipWithM_ #-}
izipWithM_ f (Vec as) (Vec bs) = G.izipWithM_ (unsafeCoerce f) as bs

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

filter :: (Vector v a) => (a -> Bool) -> Vec v a n -> Sigma Nat (TyCon1 (Vec v a))
{-# INLINE filter #-}
filter f (Vec v) = fromVector (G.filter f v)

ifilter :: (Vector v a) => (Bound n -> a -> Bool) -> Vec v a n -> Sigma Nat (TyCon1 (Vec v a))
{-# INLINE ifilter #-}
ifilter f (Vec v) = fromVector (G.ifilter (coerce f) v)

filterM :: (Vector v a, Monad m) => (a -> m Bool) -> Vec v a n -> m (Sigma Nat (TyCon1 (Vec v a)))
{-# INLINE filterM #-}
filterM f (Vec v) = fmap fromVector (G.filterM f v)

takeWhile :: (Vector v a) => (a -> Bool) -> Vec v a n -> Sigma Nat (TyCon1 (Vec v a))
{-# INLINE takeWhile #-}
takeWhile f (Vec v) = fromVector (G.takeWhile f v)

dropWhile :: (Vector v a) => (a -> Bool) -> Vec v a n -> Sigma Nat (TyCon1 (Vec v a))
{-# INLINE dropWhile #-}
dropWhile f (Vec v) = fromVector (G.dropWhile f v)

-- TODO: the post condition on partition is that xs :+ ys = zs
partition :: (Vector v a) => (a -> Bool) -> Vec v a n -> (Sigma Nat (TyCon1 (Vec v a)), Sigma Nat (TyCon1 (Vec v a)))
{-# INLINE partition #-}
partition f (Vec v) = bimap fromVector fromVector (G.partition f v)

unstablePartition :: (Vector v a) => (a -> Bool) -> Vec v a n -> (Sigma Nat (TyCon1 (Vec v a)), Sigma Nat (TyCon1 (Vec v a)))
{-# INLINE unstablePartition #-}
unstablePartition f (Vec v) = bimap fromVector fromVector (G.unstablePartition f v)

span :: (Vector v a) => (a -> Bool) -> Vec v a n -> (Sigma Nat (TyCon1 (Vec v a)), Sigma Nat (TyCon1 (Vec v a)))
{-# INLINE span #-}
span f (Vec v) = bimap fromVector fromVector (G.span f v)

break :: (Vector v a) => (a -> Bool) -> Vec v a n -> (Sigma Nat (TyCon1 (Vec v a)), Sigma Nat (TyCon1 (Vec v a)))
{-# INLINE break #-}
break f (Vec v) = bimap fromVector fromVector (G.break f v)

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

findIndex :: (Vector v a) => (a -> Bool) -> Vec v a n -> Maybe (Bound n)
{-# INLINE findIndex #-}
findIndex f (Vec v) = coerce (G.findIndex f v)

findIndices :: forall n v a. (Vector v a, Vector v Int, Vector v (Bound n))
  => (a -> Bool) -> Vec v a n -> Sigma Nat (TyCon1 (Vec v (Bound n)))
{-# INLINE findIndices #-}
findIndices f (Vec v) = fromVector (fins (G.findIndices f v))

elemIndex :: (Vector v a, Eq a) => a -> Vec v a n -> Maybe (Bound n)
{-# INLINE elemIndex #-}
elemIndex x (Vec v) = coerce (G.elemIndex x v)

elemIndices :: (Vector v a, Vector v Int, Vector v (Bound n), Eq a) =>
  a -> Vec v a n -> Sigma Nat (TyCon1 (Vec v (Bound n)))
{-# INLINE elemIndices #-}
elemIndices x (Vec v) = fromVector (fins (G.elemIndices x v))

foldl :: (Vector v b) => (a -> b -> a) -> a -> Vec v b n -> a
{-# INLINE foldl #-}
foldl f x (Vec v) = G.foldl f x v

foldl1 :: (Vector v a) => (a -> a -> a) -> Vec v a (S n) -> a
{-# INLINE foldl1 #-}
foldl1 f (Vec v) = G.foldl1 f v

foldl' :: (Vector v b) => (a -> b -> a) -> a -> Vec v b n -> a
{-# INLINE foldl' #-}
foldl' f x (Vec v) = G.foldl' f x v

foldl1' :: (Vector v a) => (a -> a -> a) -> Vec v a (S n) -> a
{-# INLINE foldl1' #-}
foldl1' f (Vec v) = G.foldl1' f v

foldlD :: (Vector v b) => (a l -> b -> a (S l)) -> a Z -> Vec v b n -> a n
{-# INLINE foldlD #-}
foldlD f x (Vec v) = G.foldl (unsafeCoerce f) (unsafeCoerce x) v

foldlD' :: (Vector v b) => (a l -> b -> a (S l)) -> a Z -> Vec v b n -> a n
{-# INLINE foldlD' #-}
foldlD' f x (Vec v) = G.foldl' (unsafeCoerce f) (unsafeCoerce x) v

foldr :: (Vector v a) => (a -> b -> b) -> b -> Vec v a n -> b
{-# INLINE foldr #-}
foldr f x (Vec v) = G.foldr f x v

foldr1 :: (Vector v a) => (a -> a -> a) -> Vec v a (S n) -> a
{-# INLINE foldr1 #-}
foldr1 f (Vec v) = G.foldr1 f v

foldr' :: (Vector v a) => (a -> b -> b) -> b -> Vec v a n -> b
{-# INLINE foldr' #-}
foldr' f x (Vec v) = G.foldr' f x v

foldr1' :: (Vector v a) => (a -> a -> a) -> Vec v a (S n) -> a
{-# INLINE foldr1' #-}
foldr1' f (Vec v) = G.foldr1' f v

foldrD :: (Vector v a) => (a -> b l -> b (S l)) -> b Z -> Vec v a n -> b n
{-# INLINE foldrD #-}
foldrD f x (Vec v) = G.foldr (unsafeCoerce f) (unsafeCoerce x) v

foldrD' :: (Vector v a) => (a -> b l -> b (S l)) -> b Z -> Vec v a n -> b n
{-# INLINE foldrD' #-}
foldrD' f x (Vec v) = G.foldr' (unsafeCoerce f) (unsafeCoerce x) v

ifoldl :: (Vector v b) => (a -> Bound n -> b -> a) -> a -> Vec v b n -> a
{-# INLINE ifoldl #-}
ifoldl f x (Vec v) = G.ifoldl (coerce f) x v

ifoldl' :: (Vector v b) => (a -> Bound n -> b -> a) -> a -> Vec v b n -> a
{-# INLINE ifoldl' #-}
ifoldl' f x (Vec v) = G.ifoldl' (coerce f) x v

ifoldlD :: (Vector v b) => (a l -> Bound n -> b -> a (S l)) -> a Z -> Vec v b n -> a n
{-# INLINE ifoldlD #-}
ifoldlD f x (Vec v) = G.ifoldl (unsafeCoerce f) (unsafeCoerce x) v

ifoldlD' :: (Vector v b) => (a l -> Bound n -> b -> a (S l)) -> a Z -> Vec v b n -> a n
{-# INLINE ifoldlD' #-}
ifoldlD' f x (Vec v) = G.ifoldl' (unsafeCoerce f) (unsafeCoerce x) v

ifoldr :: (Vector v a) => (Bound n -> a -> b -> b) -> b -> Vec v a n -> b
{-# INLINE ifoldr #-}
ifoldr f x (Vec v) = G.ifoldr (coerce f) x v

ifoldr' :: (Vector v a) => (Bound n -> a -> b -> b) -> b -> Vec v a n -> b
{-# INLINE ifoldr' #-}
ifoldr' f x (Vec v) = G.ifoldr' (coerce f) x v

ifoldrD :: (Vector v a) => (Bound n -> a -> b l -> b (S l)) -> b Z -> Vec v a n -> b n
{-# INLINE ifoldrD #-}
ifoldrD f x (Vec v) = G.ifoldr (unsafeCoerce f) (unsafeCoerce x) v

ifoldrD' :: (Vector v a) => (Bound n -> a -> b l -> b (S l)) -> b Z -> Vec v a n -> b n
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

maximum :: (Vector v a, Ord a) => Vec v a (S n) -> a
{-# INLINE maximum #-}
maximum (Vec v) = G.maximum v

maximumBy :: (Vector v a) => (a -> a -> Ordering) -> Vec v a (S n) -> a
{-# INLINE maximumBy #-}
maximumBy c (Vec v) = G.maximumBy c v

minimum :: (Vector v a, Ord a) => Vec v a (S n) -> a
{-# INLINE minimum #-}
minimum (Vec v) = G.minimum v

minimumBy :: (Vector v a) => (a -> a -> Ordering) -> Vec v a (S n) -> a
{-# INLINE minimumBy #-}
minimumBy c (Vec v) = G.minimumBy c v

minIndex :: (Vector v a, Ord a) => Vec v a (S n) -> Bound (S n)
{-# INLINE minIndex #-}
minIndex (Vec v) = Bound (G.minIndex v)

minIndexBy :: (Vector v a) => (a -> a -> Ordering) -> Vec v a (S n) -> Bound (S n)
{-# INLINE minIndexBy #-}
minIndexBy c (Vec v) = Bound (G.minIndexBy c v)

maxIndex :: (Vector v a, Ord a) => Vec v a (S n) -> Bound (S n)
{-# INLINE maxIndex #-}
maxIndex (Vec v) = Bound (G.maxIndex v)

maxIndexBy :: (Vector v a) => (a -> a -> Ordering) -> Vec v a (S n) -> Bound (S n)
{-# INLINE maxIndexBy #-}
maxIndexBy c (Vec v) = Bound (G.maxIndexBy c v)

foldM :: (Vector v b, Monad m) => (a -> b -> m a) -> a -> Vec v b n -> m a
{-# INLINE foldM #-}
foldM f x (Vec v) = G.foldM f x v

foldM' :: (Vector v b, Monad m) => (a -> b -> m a) -> a -> Vec v b n -> m a
{-# INLINE foldM' #-}
foldM' f x (Vec v) = G.foldM' f x v

ifoldM :: (Vector v b, Monad m) => (a -> Bound n -> b -> m a) -> a -> Vec v b n -> m a
{-# INLINE ifoldM #-}
ifoldM f x (Vec v) = G.ifoldM (coerce f) x v

ifoldM' :: (Vector v b, Monad m) => (a -> Bound n -> b -> m a) -> a -> Vec v b n -> m a
{-# INLINE ifoldM' #-}
ifoldM' f x (Vec v) = G.ifoldM' (coerce f) x v

fold1M :: (Vector v a, Monad m) => (a -> a -> m a) -> Vec v a (S n) -> m a
{-# INLINE fold1M #-}
fold1M f (Vec v) = G.fold1M f v

fold1M' :: (Vector v a, Monad m) => (a -> a -> m a) -> Vec v a (S n) -> m a
{-# INLINE fold1M' #-}
fold1M' f (Vec v) = G.fold1M' f v

foldM_ :: (Vector v b, Monad m) => (a -> b -> m a) -> a -> Vec v b n -> m ()
{-# INLINE foldM_ #-}
foldM_ f x (Vec v) = G.foldM_ f x v

foldM'_ :: (Vector v b, Monad m) => (a -> b -> m a) -> a -> Vec v b n -> m ()
{-# INLINE foldM'_ #-}
foldM'_ f x (Vec v) = G.foldM'_ f x v

ifoldM_ :: (Vector v b, Monad m) => (a -> Bound n -> b -> m a) -> a -> Vec v b n -> m ()
{-# INLINE ifoldM_ #-}
ifoldM_ f x (Vec v) = G.ifoldM_ (coerce f) x v

ifoldM'_ :: (Vector v b, Monad m) => (a -> Bound n -> b -> m a) -> a -> Vec v b n -> m ()
{-# INLINE ifoldM'_ #-}
ifoldM'_ f x (Vec v) = G.ifoldM'_ (coerce f) x v

fold1M_ :: (Vector v a, Monad m) => (a -> a -> m a) -> Vec v a (S n) -> m ()
{-# INLINE fold1M_ #-}
fold1M_ f (Vec v) = G.fold1M_ f v

fold1M'_ :: (Vector v a, Monad m) => (a -> a -> m a) -> Vec v a (S n) -> m ()
{-# INLINE fold1M'_ #-}
fold1M'_ f (Vec v) = G.fold1M'_ f v

foldMD :: (Vector v b, Monad m) => (a l -> b -> a (S l)) -> a Z -> Vec v b n -> m (a n)
{-# INLINE foldMD #-}
foldMD f x (Vec v) = G.foldM (unsafeCoerce f) (unsafeCoerce x) v

foldMD' :: (Vector v b, Monad m) => (a l -> b -> a (S l)) -> a Z -> Vec v b n -> m (a n)
{-# INLINE foldMD' #-}
foldMD' f x (Vec v) = G.foldM' (unsafeCoerce f) (unsafeCoerce x) v

ifoldMD :: (Vector v b, Monad m) => (a l -> Bound n -> b -> a (S l)) -> a Z -> Vec v b n -> m (a n)
{-# INLINE ifoldMD #-}
ifoldMD f x (Vec v) = G.ifoldM (unsafeCoerce f) (unsafeCoerce x) v

ifoldMD' :: (Vector v b, Monad m) => (a l -> Bound n -> b -> a (S l)) -> a Z -> Vec v b n -> m (a n)
{-# INLINE ifoldMD' #-}
ifoldMD' f x (Vec v) = G.ifoldM' (unsafeCoerce f) (unsafeCoerce x) v

sequence :: (Vector v a, Vector v (m a), Monad m) => Vec v (m a) n -> m (Vec v a n)
{-# INLINE sequence #-}
sequence (Vec v) = fmap Vec (G.sequence v)

sequence_ :: (Vector v (m a), Monad m) => Vec v (m a) n -> m ()
{-# INLINE sequence_ #-}
sequence_ (Vec v) = G.sequence_ v

prescanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> Vec v b n -> Vec v a n
{-# INLINE prescanl #-}
prescanl f x (Vec v) = Vec (G.prescanl f x v)

prescanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> Vec v b n -> Vec v a n
{-# INLINE prescanl' #-}
prescanl' f x (Vec v) = Vec (G.prescanl' f x v)

postscanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> Vec v b n -> Vec v a n
{-# INLINE postscanl #-}
postscanl f x (Vec v) = Vec (G.postscanl f x v)

postscanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> Vec v b n -> Vec v a n
{-# INLINE postscanl' #-}
postscanl' f x (Vec v) = Vec (G.postscanl' f x v)

scanl :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> Vec v b n -> Vec v a (S n)
{-# INLINE scanl #-}
scanl f x (Vec v) = Vec (G.scanl f x v)

scanl' :: (Vector v a, Vector v b) => (a -> b -> a) -> a -> Vec v b n -> Vec v a (S n)
{-# INLINE scanl' #-}
scanl' f x (Vec v) = Vec (G.scanl' f x v)

scanl1 :: (Vector v a) => (a -> a -> a) -> Vec v a (S n) -> Vec v a (S n)
{-# INLINE scanl1 #-}
scanl1 f (Vec v) = Vec (G.scanl1 f v)

scanl1' :: (Vector v a) => (a -> a -> a) -> Vec v a (S n) -> Vec v a (S n)
{-# INLINE scanl1' #-}
scanl1' f (Vec v) = Vec (G.scanl1' f v)

prescanr :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> Vec v a n -> Vec v b n
{-# INLINE prescanr #-}
prescanr f x (Vec v) = Vec (G.prescanr f x v)

prescanr' :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> Vec v a n -> Vec v b n
{-# INLINE prescanr' #-}
prescanr' f x (Vec v) = Vec (G.prescanr' f x v)

postscanr :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> Vec v a n -> Vec v b n
{-# INLINE postscanr #-}
postscanr f x (Vec v) = Vec (G.postscanr f x v)

postscanr' :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> Vec v a n -> Vec v b n
{-# INLINE postscanr' #-}
postscanr' f x (Vec v) = Vec (G.postscanr' f x v)

scanr :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> Vec v a n -> Vec v b (S n)
{-# INLINE scanr #-}
scanr f x (Vec v) = Vec (G.scanr f x v)

scanr' :: (Vector v a, Vector v b) => (a -> b -> b) -> b -> Vec v a n -> Vec v b (S n)
{-# INLINE scanr' #-}
scanr' f x (Vec v) = Vec (G.scanr' f x v)

scanr1 :: (Vector v a) => (a -> a -> a) -> Vec v a n -> Vec v a n
{-# INLINE scanr1 #-}
scanr1 f (Vec v) = Vec (G.scanr1 f v)

scanr1' :: (Vector v a) => (a -> a -> a) -> Vec v a n -> Vec v a n
{-# INLINE scanr1' #-}
scanr1' f (Vec v) = Vec (G.scanr1' f v)

toList :: (Vector v a) => Vec v a n -> [a]
{-# INLINE toList #-}
toList (Vec v) = G.toList v

fromList :: (Vector v a) => [a] -> Sigma Nat (TyCon1 (Vec v a))
{-# INLINE fromList #-}
fromList list = fromVector (G.fromList list)

fromListN :: (Vector v a) => SNat n -> [a] -> Maybe (Vec v a n)
{-# INLINE fromListN #-}
fromListN (SNat len) list = fmap (Vec . G.fromList) (parseList 0 list)
  where parseList amount []
          | len <= amount = Just []
          | otherwise     = Nothing
        parseList amount (x:xs)
          | len <= amount = Just []
          | otherwise     = fmap (x:) (parseList (amount + 1) xs)

fromListN' :: (Vector v a, SingI n) => [a] -> Maybe (Vec v a n)
{-# INLINE fromListN' #-}
fromListN' list = fromListN sing list

toVector :: Vec v a n -> v a
{-# INLINE toVector #-}
toVector (Vec v) = v

fromVector :: (Vector v a) => v a -> Sigma Nat (TyCon1 (Vec v a))
{-# INLINE fromVector #-}
fromVector v = Sigma (\p -> p (SNat (G.length v)) (Vec v))

fromVectorN :: (Vector v a) => SNat n -> v a -> Maybe (Vec v a n)
{-# INLINE fromVectorN #-}
fromVectorN (SNat len) v
  | len <= G.length v = Just (Vec (G.take len v))
  | otherwise = Nothing

fromVectorN' :: (Vector v a, SingI n) => v a -> Maybe (Vec v a n)
{-# INLINE fromVectorN' #-}
fromVectorN' v = fromVectorN sing v

convert :: (Vector v a, Vector w a) => Vec v a n -> Vec w a n
{-# INLINE convert #-}
convert (Vec v) = Vec (G.convert v)

freeze :: (Vector v a, PrimMonad m) => MVec (G.Mutable v) (PrimState m) a n -> m (Vec v a n)
{-# INLINE freeze #-}
freeze (MVec mv) = strengthenVecM (G.freeze mv)

thaw :: (Vector v a, PrimMonad m) => Vec v a n -> m (MVec (G.Mutable v) (PrimState m) a n)
{-# INLINE thaw #-}
thaw (Vec v) = strengthenMVecM (G.thaw v)

copy :: (Vector v a, PrimMonad m) => MVec (G.Mutable v) (PrimState m) a n -> Vec v a n -> m ()
{-# INLINE copy #-}
copy (MVec mv) (Vec v) = G.unsafeCopy mv v

unsafeFreeze :: (Vector v a, PrimMonad m) => MVec (G.Mutable v) (PrimState m) a n -> m (Vec v a n)
{-# INLINE unsafeFreeze #-}
unsafeFreeze (MVec mv) = strengthenVecM (G.unsafeFreeze mv)

unsafeThaw :: (Vector v a, PrimMonad m) => Vec v a n -> m (MVec (G.Mutable v) (PrimState m) a n)
{-# INLINE unsafeThaw #-}
unsafeThaw (Vec v) = strengthenMVecM (G.unsafeThaw v)

stream :: (Vector v a) => Vec v a n -> B.Bundle v a
{-# INLINE stream #-}
stream (Vec v) = G.stream v

unstream :: (Vector v a) => B.Bundle v a -> Sigma Nat (TyCon1 (Vec v a))
{-# INLINE unstream #-}
unstream b = fromVector (G.unstream b)

streamR :: (Vector v a) => Vec v a n -> B.Bundle v a
{-# INLINE streamR #-}
streamR (Vec v) = G.streamR v

unstreamR :: (Vector v a) => B.Bundle v a -> Sigma Nat (TyCon1 (Vec v a))
{-# INLINE unstreamR #-}
unstreamR b = fromVector (G.unstreamR b)

new :: (Vector v a) => New v a n -> Vec v a n
{-# INLINE new #-}
new (New n) = Vec (G.new n)

clone :: (Vector v a) => Vec v a n -> New v a n
clone (Vec v) = New (G.clone v)

eq :: (Vector v a, Eq a) => Vec v a n -> Vec v a n -> Bool
{-# INLINE eq #-}
eq (Vec v) (Vec w) = G.eq v w

cmp :: (Vector v a, Ord a) => Vec v a n -> Vec v a n -> Ordering
{-# INLINE cmp #-}
cmp (Vec v) (Vec w) = G.cmp v w

showsPrec :: (Vector v a, Show a) => Int -> Vec v a n -> ShowS
{-# INLINE showsPrec #-}
showsPrec p (Vec v) = G.showsPrec p v
