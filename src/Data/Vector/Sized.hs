{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Data.Vector.Sized where

import Prelude (Maybe(..),Monad,Num(..),(.),
                Eq(..),Show(..),Monoid(..),Ord(..),
                Ordering,Bool)

import qualified Data.Vector as V (Vector)
import qualified Data.Vector.Generic as GV (Vector)
import qualified Data.Vector.Generic.Internal.Sized as G
import qualified Data.Vector.Generic.Sized as G
import Data.Sigma (Sigma(..))

import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

import Data.Nat
import Data.Fin (Fin)

newtype Vec a (n :: Nat) = Vec { getVec :: G.Vec V.Vector a n }

instance (Eq a) => Eq (Vec a n) where
  {-# INLINE (==) #-}
  Vec v == Vec w = G.eq v w

instance (Ord a) => Ord (Vec a n) where
  {-# INLINE compare #-}
  compare (Vec v) (Vec w) = G.cmp v w

instance (Monoid a, IsNat n) => Monoid (Vec a n) where
  {-# INLINE mempty #-}
  mempty = replicate' mempty
  {-# INLINE mappend #-}
  mappend = zipWith mappend

instance (Show a) => Show (Vec a n) where
  {-# INLINE showsPrec #-}
  showsPrec p (Vec v) = G.showsPrec p v

-- These functions help the role? checker
tightenSigma :: Sigma (G.Vec V.Vector a) -> Sigma (Vec a)
{-# INLINE tightenSigma #-}
tightenSigma = coerce

tightenSigmaM :: m (Sigma (G.Vec V.Vector a)) -> m (Sigma (Vec a))
{-# INLINE tightenSigmaM #-}
tightenSigmaM = unsafeCoerce

tightenSigma2 :: (Sigma (G.Vec V.Vector a), Sigma (G.Vec V.Vector a)) -> (Sigma (Vec a), Sigma (Vec a))
{-# INLINE tightenSigma2 #-}
tightenSigma2 = coerce

tightenMaybe :: Maybe (G.Vec V.Vector a n) -> Maybe (Vec a n)
{-# INLINE tightenMaybe #-}
tightenMaybe = coerce

tightenMonad :: m (G.Vec V.Vector a n) -> m (Vec a n)
{-# INLINE tightenMonad #-}
tightenMonad = unsafeCoerce

length :: Vec a n -> SNat n
{-# INLINE length #-}
length (Vec v) = G.length v

null :: Vec a n -> Maybe (IsZero n)
{-# INLINE null #-}
null (Vec v) = G.null v

infixr 9 !
(!) :: Vec a n -> Fin n -> a
{-# INLINE (!) #-}
(Vec v) ! i = v G.! i

head :: Vec a ('S n) -> a
{-# INLINE head #-}
head (Vec v) = G.head v

last :: Vec a ('S n) -> a
{-# INLINE last #-}
last (Vec v) = G.last v

indexM :: (Monad m) => Vec a n -> Fin n -> m a
{-# INLINE indexM #-}
indexM (Vec v) i = G.indexM v i

headM :: (Monad m) => Vec a ('S n)  -> m a
{-# INLINE headM #-}
headM (Vec v) = G.headM v

lastM :: (Monad m) => Vec a ('S n) -> m a
{-# INLINE lastM #-}
lastM (Vec v) = G.lastM v

slice :: SNat i -> Vec a (i + n) -> Vec a n
{-# INLINE slice #-}
slice start (Vec v) = Vec (G.slice start v)

init :: Vec a ('S n) -> Vec a n
{-# INLINE init #-}
init (Vec v) = Vec (G.init v)

tail :: Vec a ('S s) -> Vec a n
{-# INLINE tail #-}
tail (Vec v) = Vec (G.tail v)

drop :: SNat i -> Vec a (i + n) -> Vec a n
{-# INLINE drop #-}
drop amount (Vec v) = Vec (G.drop amount v)

empty :: Vec a 'Z
{-# INLINE empty #-}
empty = Vec G.empty

singleton :: a -> Vec a ('S 'Z)
{-# INLINE singleton #-}
singleton x = Vec (G.singleton x)

replicate :: SNat n -> a -> Vec a n
{-# INLINE replicate #-}
replicate amount x = Vec (G.replicate amount x)

replicate' :: (IsNat n) => a -> Vec a n
{-# INLINE replicate' #-}
replicate' x = Vec (G.replicate' x)

replicateM :: (Monad m) => SNat n -> m a -> m (Vec a n)
{-# INLINE replicateM #-}
replicateM amount x = tightenMonad (G.replicateM amount x)

replicateM' :: (Monad m, IsNat n) => m a -> m (Vec a n)
{-# INLINE replicateM' #-}
replicateM' x = tightenMonad (G.replicateM' x)

generate :: SNat n -> (Fin n -> a) -> Vec a n
{-# INLINE generate  #-}
generate amount f = Vec (G.generate amount f)

generate' :: (IsNat n) => (Fin n -> a) -> Vec a n
{-# INLINE generate' #-}
generate' f = Vec (G.generate' f)

generateM :: (Monad m) => SNat n -> (Fin n -> m a) -> m (Vec a n)
{-# INLINE generateM #-}
generateM amount f = tightenMonad (G.generateM amount f)

generateM' :: (Monad m, IsNat n) => (Fin n -> m a) -> m (Vec a n)
{-# INLINE generateM' #-}
generateM' f = tightenMonad (G.generateM' f)

allFin :: SNat n -> Vec (Fin n) n
{-# INLINE allFin #-}
allFin len = Vec (G.allFin len)

allFin' :: (IsNat n) => Vec (Fin n) n
{-# INLINE allFin' #-}
allFin' = Vec (G.allFin')

iterate :: SNat n -> (a -> a) -> a -> Vec a n
{-# INLINE iterate #-}
iterate amount f x = Vec (G.iterate amount f x)

iterate' :: (IsNat n) => (a -> a) -> a -> Vec a n
{-# INLINE iterate' #-}
iterate' f x = Vec (G.iterate' f x)

-- create

unfoldr :: (b -> Maybe (a,b)) -> b -> Sigma (Vec a)
{-# INLINE unfoldr #-}
unfoldr f x = tightenSigma (G.unfoldr f x)

unfoldrN :: SNat n -> (b -> Maybe (a,b)) -> b -> Maybe (Vec a n)
{-# INLINE unfoldrN #-}
unfoldrN amount f x = tightenMaybe (G.unfoldrN amount f x)

construct :: SNat n -> (forall m. Vec a m -> a) -> Vec a n
{-# INLINE construct #-}
construct amount f = Vec (G.construct amount (f . Vec))

construct' :: (IsNat n) => (forall m. Vec a m -> a) -> Vec a n
{-# INLINE construct' #-}
construct' f = Vec (G.construct' (f . Vec))

constructr :: SNat n -> (forall m. Vec a m -> a) -> Vec a n
{-# INLINE constructr #-}
constructr amount f = Vec (G.constructr amount (f . Vec))

constructr' :: (IsNat n) => (forall m. Vec a m -> a) -> Vec a n
{-# INLINE constructr' #-}
constructr' f = Vec (G.constructr' (f . Vec))

enumFromN :: (Num a) => a -> SNat n -> Vec a n
{-# INLINE enumFromN #-}
enumFromN x len = Vec (G.enumFromN x len)

enumFromN' :: (IsNat n, Num a) => a -> Vec a n
{-# INLINE enumFromN' #-}
enumFromN' x = Vec (G.enumFromN' x)

enumFromStepN :: (Num a) => a -> a -> SNat n -> Vec a n
{-# INLINE enumFromStepN #-}
enumFromStepN x y len = Vec (G.enumFromStepN x y len)

enumFromStepN' :: (IsNat n, Num a) => a -> a -> Vec a n
{-# INLINE enumFromStepN' #-}
enumFromStepN' x y = Vec (G.enumFromStepN' x y)

cons :: a -> Vec a n -> Vec a ('S n)
{-# INLINE cons #-}
cons x (Vec v) = Vec (G.cons x v)

snoc :: Vec a n -> a -> Vec a ('S n)
{-# INLINE snoc #-}
snoc (Vec v) x = Vec (G.snoc v x)

infixr 5 ++
(++) :: Vec a n -> Vec a m -> Vec a (n + m)
{-# INLINE (++) #-}
(Vec v) ++ (Vec w) = Vec (v G.++ w)

concat :: Vec (Vec a m) n -> Vec a (n * m)
{-# INLINE concat #-}
concat (Vec vs) = Vec (G.concat (G.map getVec vs))

force :: Vec a n -> Vec a n
{-# INLINE force #-}
force (Vec v) = Vec (G.force v)

(//) :: Vec a n -> [(Fin n, a)] -> Vec a n
{-# INLINE (//) #-}
(Vec v) // us = Vec (v G.// us)

update :: Vec a n -> Vec (Fin n, a) m -> Vec a n
{-# INLINE update #-}
update (Vec v) (Vec w) = Vec (G.update v w)

update_ :: Vec a n -> Vec (Fin n) m -> Vec a m -> Vec a n
{-# INLINE update_ #-}
update_ (Vec v) (Vec w) (Vec x) = Vec (G.update_ v w x)

accum :: (a -> b -> a) -> Vec a n -> [(Fin n, b)] -> Vec a n
{-# INLINE accum #-}
accum f (Vec v) us = Vec (G.accum f v us)

accumulate :: (a -> b -> a) -> Vec a n -> Vec (Fin n, b) m -> Vec a n
{-# INLINE accumulate #-}
accumulate f (Vec v) (Vec w) = Vec (G.accumulate f v w)

accumulate_ :: (a -> b -> a) -> Vec a n -> Vec (Fin n) m -> Vec b m -> Vec a n
{-# INLINE accumulate_ #-}
accumulate_ f (Vec v) (Vec w) (Vec x) = Vec (G.accumulate_ f v w x)

reverse :: Vec a n -> Vec a n
{-# INLINE reverse #-}
reverse (Vec v) = Vec (G.reverse v)

backpermute :: Vec a n -> Vec (Fin n) m -> Vec a m
{-# INLINE backpermute #-}
backpermute (Vec v) (Vec w) = Vec (G.backpermute v w)

indexed :: Vec a n -> Vec (Fin n, a) n
{-# INLINE indexed #-}
indexed (Vec v) = Vec (G.indexed v)

diagonal :: (IsNat n) => Vec (Vec a n) n -> Vec a n
{-# INLINE diagonal #-}
diagonal (Vec v) = Vec (G.diagonal (coerce v))

map :: (a -> b) -> Vec a n -> Vec b n
{-# INLINE map #-}
map f (Vec v) = Vec (G.map f v)

imap :: (Fin n -> a -> b) -> Vec a n -> Vec b n
{-# INLINE imap #-}
imap f (Vec v) = Vec (G.imap f v)

concatMap :: (a -> Vec b n) -> Vec a m -> Vec b (n * m)
{-# INLINE concatMap #-}
concatMap f (Vec v) = Vec (G.concatMap (getVec . f) v)

mapM :: (Monad m) => (a -> m b) -> Vec a n -> m (Vec b n)
{-# INLINE mapM #-}
mapM f (Vec v) = tightenMonad (G.mapM f v)

imapM :: (Monad m) => (Fin n -> a -> m b) -> Vec a n -> m (Vec b n)
{-# INLINE imapM #-}
imapM f (Vec v) = tightenMonad (G.imapM f v)

mapM_ :: (Monad m) => (a -> m b) -> Vec a n -> m ()
{-# INLINE mapM_ #-}
mapM_ f (Vec v) = G.mapM_ f v

imapM_ :: (Monad m) => (Fin n -> a -> m b) -> Vec a n -> m ()
{-# INLINE imapM_ #-}
imapM_ f (Vec v) = G.imapM_ f v

forM :: (Monad m) => Vec a n -> (a -> m b) -> m (Vec b n)
{-# INLINE forM #-}
forM (Vec v) f = tightenMonad (G.mapM f v)

forM_ :: (Monad m) => Vec a n -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ (Vec v) f = G.mapM_ f v

zipWith :: () =>
  (a -> b -> c) ->
  Vec a n -> Vec b n -> Vec c n
{-# INLINE zipWith #-}
zipWith f (Vec as) (Vec bs) = Vec (G.zipWith f as bs)

zipWith3 :: () =>
  (a -> b -> c -> d) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n
{-# INLINE zipWith3 #-}
zipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.zipWith3 f as bs cs)

zipWith4 :: () =>
  (a -> b -> c -> d -> e) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n
{-# INLINE zipWith4 #-}
zipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zipWith4 f as bs cs ds)

zipWith5 :: () =>
  (a -> b -> c -> d -> e -> f) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n -> Vec f n
{-# INLINE zipWith5 #-}
zipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zipWith5 f as bs cs ds es)

zipWith6 :: () =>
  (a -> b -> c -> d -> e -> f -> g) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n -> Vec f n -> Vec g n
{-# INLINE zipWith6 #-}
zipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zipWith6 f as bs cs ds es fs)

izipWith :: () =>
  (Fin n -> a -> b -> c) ->
  Vec a n -> Vec b n -> Vec c n
{-# INLINE izipWith #-}
izipWith f (Vec as) (Vec bs) = Vec (G.izipWith f as bs)

izipWith3 :: () =>
  (Fin n -> a -> b -> c -> d) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n
{-# INLINE izipWith3 #-}
izipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.izipWith3 f as bs cs)

izipWith4 :: () =>
  (Fin n -> a -> b -> c -> d -> e) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n
{-# INLINE izipWith4 #-}
izipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.izipWith4 f as bs cs ds)

izipWith5 :: () =>
  (Fin n -> a -> b -> c -> d -> e -> f) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n -> Vec f n
{-# INLINE izipWith5 #-}
izipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.izipWith5 f as bs cs ds es)

izipWith6 :: () =>
  (Fin n -> a -> b -> c -> d -> e -> f -> g) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n -> Vec f n -> Vec g n
{-# INLINE izipWith6 #-}
izipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.izipWith6 f as bs cs ds es fs)

zip :: () =>
  Vec a n -> Vec b n -> Vec (a, b) n
{-# INLINE zip #-}
zip (Vec as) (Vec bs) = Vec (G.zip as bs)

zip3 :: () =>
  Vec a n -> Vec b n -> Vec c n -> Vec (a, b, c) n
{-# INLINE zip3 #-}
zip3 (Vec as) (Vec bs) (Vec cs) = Vec (G.zip3 as bs cs)

zip4 :: () =>
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec (a, b, c, d) n
{-# INLINE zip4 #-}
zip4 (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zip4 as bs cs ds)

zip5 :: () =>
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n -> Vec (a, b, c, d, e) n
{-# INLINE zip5 #-}
zip5 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zip5 as bs cs ds es)

zip6 :: () =>
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n -> Vec f n -> Vec (a, b, c, d, e, f) n
{-# INLINE zip6 #-}
zip6 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zip6 as bs cs ds es fs)

unzip :: () =>
  Vec (a, b) n -> (Vec a n, Vec b n)
{-# INLINE unzip #-}
unzip (Vec vs) = (Vec as, Vec bs)
  where (as, bs) = G.unzip vs

unzip3 :: () =>
  Vec (a, b, c) n -> (Vec a n, Vec b n, Vec c n)
{-# INLINE unzip3 #-}
unzip3 (Vec vs) = (Vec as, Vec bs, Vec cs)
  where (as, bs, cs) = G.unzip3 vs

unzip4 :: () =>
  Vec (a, b, c, d) n -> (Vec a n, Vec b n, Vec c n, Vec d n)
{-# INLINE unzip4 #-}
unzip4 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds)
  where (as, bs, cs, ds) = G.unzip4 vs

unzip5 :: () =>
  Vec (a, b, c, d, e) n -> (Vec a n, Vec b n, Vec c n, Vec d n, Vec e n)
{-# INLINE unzip5 #-}
unzip5 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds, Vec es)
  where (as, bs, cs, ds, es) = G.unzip5 vs

unzip6 :: () =>
  Vec (a, b, c, d, e, f) n -> (Vec a n, Vec b n, Vec c n, Vec d n, Vec e n, Vec f n)
{-# INLINE unzip6 #-}
unzip6 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds, Vec es, Vec fs)
  where (as, bs, cs, ds, es, fs) = G.unzip6 vs

filter :: (a -> Bool) -> Vec a n -> Sigma (Vec a)
{-# INLINE filter #-}
filter f (Vec v) = tightenSigma (G.filter f v)

ifilter :: (Fin n -> a -> Bool) -> Vec a n -> Sigma (Vec a)
{-# INLINE ifilter #-}
ifilter f (Vec v) = tightenSigma (G.ifilter f v)

filterM :: (Monad m) => (a -> m Bool) -> Vec a n -> m (Sigma (Vec a))
{-# INLINE filterM #-}
filterM f (Vec v) = tightenSigmaM (G.filterM f v)

takeWhile :: (a -> Bool) -> Vec a n -> Sigma (Vec a)
{-# INLINE takeWhile #-}
takeWhile f (Vec v) = tightenSigma (G.takeWhile f v)

dropWhile :: (a -> Bool) -> Vec a n -> Sigma (Vec a)
{-# INLINE dropWhile #-}
dropWhile f (Vec v) = tightenSigma (G.dropWhile f v)

partition :: (a -> Bool) -> Vec a n -> (Sigma (Vec a), Sigma (Vec a))
{-# INLINE partition #-}
partition f (Vec v) = tightenSigma2 (G.partition f v)

unstabelPartition :: (a -> Bool) -> Vec a n -> (Sigma (Vec a), Sigma (Vec a))
{-# INLINE unstabelPartition #-}
unstabelPartition f (Vec v) = tightenSigma2 (G.unstablePartition f v)

span :: (a -> Bool) -> Vec a n -> (Sigma (Vec a), Sigma (Vec a))
{-# INLINE span #-}
span f (Vec v) = tightenSigma2 (G.span f v)

break :: (a -> Bool) -> Vec a n -> (Sigma (Vec a), Sigma (Vec a))
{-# INLINE break #-}
break f (Vec v) = tightenSigma2 (G.break f v)

infix 4 `elem`
elem :: (Eq a) => a -> Vec a n -> Bool
{-# INLINE elem #-}
elem x (Vec v) = G.elem x v

notElem :: (Eq a) => a -> Vec a n -> Bool
{-# INLINE notElem #-}
notElem x (Vec v) = G.notElem x v

find :: (a -> Bool) -> Vec a n -> Maybe a
{-# INLINE find #-}
find f (Vec v) = G.find f v

findIndex :: (a -> Bool) -> Vec a n -> Maybe (Fin n)
{-# INLINE findIndex #-}
findIndex f (Vec v) = G.findIndex f v

findIndices :: (a -> Bool) -> Vec a n -> Sigma (Vec (Fin n))
{-# INLINE findIndices #-}
findIndices f (Vec v) = tightenSigma (G.findIndices f v)

elemIndex :: (Eq a) => a -> Vec a n -> Maybe (Fin n)
{-# INLINE elemIndex #-}
elemIndex x (Vec v) = G.elemIndex x v

elemIndices :: (Eq a) => a -> Vec a n -> Sigma (Vec (Fin n))
{-# INLINE elemIndices #-}
elemIndices x (Vec v) = tightenSigma (G.elemIndices x v)

foldl :: (a -> b -> a) -> a -> Vec b n -> a
{-# INLINE foldl #-}
foldl f x (Vec v) = G.foldl f x v

foldl1 :: (a -> a -> a) -> Vec a ('S n) -> a
{-# INLINE foldl1 #-}
foldl1 f (Vec v) = G.foldl1 f v

foldl' :: (a -> b -> a) -> a -> Vec b n -> a
{-# INLINE foldl' #-}
foldl' f x (Vec v) = G.foldl' f x v

foldl1' :: (a -> a -> a) -> Vec a ('S n) -> a
{-# INLINE foldl1' #-}
foldl1' f (Vec v) = G.foldl1' f v

foldlD :: (a l -> b -> a ('S l)) -> a 'Z -> Vec b n -> a n
{-# INLINE foldlD #-}
foldlD f x (Vec v) = G.foldlD f x v

foldlD' :: (a l -> b -> a ('S l)) -> a 'Z -> Vec b n -> a n
{-# INLINE foldlD' #-}
foldlD' f x (Vec v) = G.foldlD' f x v

foldr :: (a -> b -> b) -> b -> Vec a n -> b
{-# INLINE foldr #-}
foldr f x (Vec v) = G.foldr f x v

foldr1 :: (a -> a -> a) -> Vec a ('S n) -> a
{-# INLINE foldr1 #-}
foldr1 f (Vec v) = G.foldr1 f v

foldr' :: (a -> b -> b) -> b -> Vec a n -> b
{-# INLINE foldr' #-}
foldr' f x (Vec v) = G.foldr' f x v

foldr1' :: (a -> a -> a) -> Vec a ('S n) -> a
{-# INLINE foldr1' #-}
foldr1' f (Vec v) = G.foldr1' f v

foldrD :: (a -> b l -> b ('S l)) -> b 'Z -> Vec a n -> b n
{-# INLINE foldrD #-}
foldrD f x (Vec v) = G.foldrD f x v

foldrD' :: (a -> b l -> b ('S l)) -> b 'Z -> Vec a n -> b n
{-# INLINE foldrD' #-}
foldrD' f x (Vec v) = G.foldrD' f x v

ifoldl :: (a -> Fin n -> b -> a) -> a -> Vec b n -> a
{-# INLINE ifoldl #-}
ifoldl f x (Vec v) = G.ifoldl f x v

ifoldl' :: (a -> Fin n -> b -> a) -> a -> Vec b n -> a
{-# INLINE ifoldl' #-}
ifoldl' f x (Vec v) = G.ifoldl' f x v

ifoldlD :: (a l -> Fin n -> b -> a ('S l)) -> a 'Z -> Vec b n -> a n
{-# INLINE ifoldlD #-}
ifoldlD f x (Vec v) = G.ifoldlD f x v

ifoldlD' :: (a l -> Fin n -> b -> a ('S l)) -> a 'Z -> Vec b n -> a n
{-# INLINE ifoldlD' #-}
ifoldlD' f x (Vec v) = G.ifoldlD' f x v

ifoldr :: (Fin n -> a -> b -> b) -> b -> Vec a n -> b
{-# INLINE ifoldr #-}
ifoldr f x (Vec v) = G.ifoldr f x v

ifoldr' :: (Fin n -> a -> b -> b) -> b -> Vec a n -> b
{-# INLINE ifoldr' #-}
ifoldr' f x (Vec v) = G.ifoldr' f x v

ifoldrD :: (Fin n -> a -> b l -> b ('S l)) -> b 'Z -> Vec a n -> b n
{-# INLINE ifoldrD #-}
ifoldrD f x (Vec v) = G.ifoldrD f x v

ifoldrD' :: (Fin n -> a -> b l -> b ('S l)) -> b 'Z -> Vec a n -> b n
{-# INLINE ifoldrD' #-}
ifoldrD' f x (Vec v) = G.ifoldrD' f x v

all :: (a -> Bool) -> Vec a n -> Bool
{-# INLINE all #-}
all p (Vec v) = G.all p v

any :: (a -> Bool) -> Vec a n -> Bool
{-# INLINE any #-}
any p (Vec v) = G.any p v

and :: Vec Bool n -> Bool
{-# INLINE and #-}
and (Vec v) = G.and v

or :: Vec Bool n -> Bool
{-# INLINE or #-}
or (Vec v) = G.or v

sum :: (Num a) => Vec a n -> a
{-# INLINE sum #-}
sum (Vec v) = G.sum v

product :: (Num a) => Vec a n -> a
{-# INLINE product #-}
product (Vec v) = G.product v

maximum :: (Ord a) => Vec a ('S n) -> a
{-# INLINE maximum #-}
maximum (Vec v) = G.maximum v

maximumBy :: (a -> a -> Ordering) -> Vec a ('S n) -> a
{-# INLINE maximumBy #-}
maximumBy c (Vec v) = G.maximumBy c v

minimum :: (Ord a) => Vec a ('S n) -> a
{-# INLINE minimum #-}
minimum (Vec v) = G.minimum v

minimumBy :: (a -> a -> Ordering) -> Vec a ('S n) -> a
{-# INLINE minimumBy #-}
minimumBy c (Vec v) = G.minimumBy c v

minIndex :: (Ord a) => Vec a ('S n) -> Fin ('S n)
{-# INLINE minIndex #-}
minIndex (Vec v) = G.minIndex v

minIndexBy :: (a -> a -> Ordering) -> Vec a ('S n) -> Fin ('S n)
{-# INLINE minIndexBy #-}
minIndexBy c (Vec v) = G.minIndexBy c v

maxIndex :: (Ord a) => Vec a ('S n) -> Fin ('S n)
{-# INLINE maxIndex #-}
maxIndex (Vec v) = G.maxIndex v

maxIndexBy :: (a -> a -> Ordering) -> Vec a ('S n) -> Fin ('S n)
{-# INLINE maxIndexBy #-}
maxIndexBy c (Vec v) = G.maxIndexBy c v

sequence :: (Monad m) => Vec (m a) n -> m (Vec a n)
{-# INLINE sequence #-}
sequence (Vec v) = tightenMonad (G.sequence v)

sequence_ :: (Monad m) => Vec (m a) n -> m ()
{-# INLINE sequence_ #-}
sequence_ (Vec v) = G.sequence_ v

toList :: Vec a n -> [a]
{-# INLINE toList #-}
toList (Vec v) = G.toList v

fromList :: [a] -> Sigma (Vec a)
{-# INLINE fromList #-}
fromList list = tightenSigma (G.fromList list)

fromListN :: SNat n -> [a] -> Maybe (Vec a n)
{-# INLINE fromListN #-}
fromListN len list = tightenMaybe (G.fromListN len list)

fromListN' :: (IsNat n) => [a] -> Maybe (Vec a n)
{-# INLINE fromListN' #-}
fromListN' list = tightenMaybe (G.fromListN' list)

toVector :: Vec a n -> V.Vector a
{-# INLINE toVector #-}
toVector (Vec v) = G.toVector v

fromVector :: V.Vector a -> Sigma (Vec a)
{-# INLINE fromVector #-}
fromVector v = tightenSigma (G.fromVector v)

fromVectorN :: SNat n -> V.Vector a -> Maybe (Vec a n)
{-# INLINE fromVectorN #-}
fromVectorN len v = coerce (G.fromVectorN len v)

fromVectorN' :: (IsNat n) => V.Vector a -> Maybe (Vec a n)
{-# INLINE fromVectorN' #-}
fromVectorN' v = tightenMaybe (G.fromVectorN' v)

convert :: (GV.Vector v a, GV.Vector w a) => G.Vec v a n -> G.Vec w a n
{-# INLINE convert #-}
convert v = G.convert v

