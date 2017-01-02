{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -Werror #-}
module Data.Vector.Unboxed.Sized where

import Prelude (Maybe(..),Monad,Num(..),(.),
                Eq(..),Ord(..),Ordering,Bool)

import Data.Vector.Unboxed (Unbox)
import Data.Vector.Unboxed.Instances ()
import qualified Data.Vector.Unboxed as U (Vector)
import qualified Data.Vector.Generic as GV (Vector)
import qualified Data.Vector.Generic.Internal.Sized as G
import qualified Data.Vector.Generic.Sized as G
import Data.Sigma (Sigma(..))

import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

import Data.Nat
import Data.Fin (Fin)

newtype Vec a (n :: Nat) = Vec { getVec :: G.Vec U.Vector a n }

-- These functions help the role? checker
tightenSigma :: Sigma (G.Vec U.Vector a) -> Sigma (Vec a)
{-# INLINE tightenSigma #-}
tightenSigma = coerce

tightenSigmaM :: m (Sigma (G.Vec U.Vector a)) -> m (Sigma (Vec a))
{-# INLINE tightenSigmaM #-}
tightenSigmaM = unsafeCoerce

tightenSigma2 :: (Sigma (G.Vec U.Vector a), Sigma (G.Vec U.Vector a)) -> (Sigma (Vec a), Sigma (Vec a))
{-# INLINE tightenSigma2 #-}
tightenSigma2 = coerce

tightenMaybe :: Maybe (G.Vec U.Vector a n) -> Maybe (Vec a n)
{-# INLINE tightenMaybe #-}
tightenMaybe = coerce

tightenMonad :: m (G.Vec U.Vector a n) -> m (Vec a n)
{-# INLINE tightenMonad #-}
tightenMonad = unsafeCoerce

length :: (Unbox a) => Vec a n -> SNat n
{-# INLINE length #-}
length (Vec v) = G.length v

null :: (Unbox a) => Vec a n -> Maybe (IsZero n)
{-# INLINE null #-}
null (Vec v) = G.null v

infixr 9 !
(!) :: (Unbox a) => Vec a n -> Fin n -> a
{-# INLINE (!) #-}
(Vec v) ! i = v G.! i

head :: (Unbox a) => Vec a ('S n) -> a
{-# INLINE head #-}
head (Vec v) = G.head v

last :: (Unbox a) => Vec a ('S n) -> a
{-# INLINE last #-}
last (Vec v) = G.last v

indexM :: (Unbox a, Monad m) => Vec a n -> Fin n -> m a
{-# INLINE indexM #-}
indexM (Vec v) i = G.indexM v i

headM :: (Unbox a, Monad m) => Vec a ('S n)  -> m a
{-# INLINE headM #-}
headM (Vec v) = G.headM v

lastM :: (Unbox a, Monad m) => Vec a ('S n) -> m a
{-# INLINE lastM #-}
lastM (Vec v) = G.lastM v

slice :: (Unbox a) => SNat i -> Vec a (i + n) -> Vec a n
{-# INLINE slice #-}
slice start (Vec v) = Vec (G.slice start v)

init :: (Unbox a) => Vec a ('S n) -> Vec a n
{-# INLINE init #-}
init (Vec v) = Vec (G.init v)

tail :: (Unbox a) => Vec a ('S s) -> Vec a n
{-# INLINE tail #-}
tail (Vec v) = Vec (G.tail v)

drop :: (Unbox a) => SNat i -> Vec a (i + n) -> Vec a n
{-# INLINE drop #-}
drop amount (Vec v) = Vec (G.drop amount v)

empty :: (Unbox a) => Vec a 'Z
{-# INLINE empty #-}
empty = Vec G.empty

singleton :: (Unbox a) => a -> Vec a ('S 'Z)
{-# INLINE singleton #-}
singleton x = Vec (G.singleton x)

replicate :: (Unbox a) => SNat n -> a -> Vec a n
{-# INLINE replicate #-}
replicate amount x = Vec (G.replicate amount x)

replicate' :: (Unbox a, IsNat n) => a -> Vec a n
{-# INLINE replicate' #-}
replicate' x = Vec (G.replicate' x)

replicateM :: (Unbox a, Monad m) => SNat n -> m a -> m (Vec a n)
{-# INLINE replicateM #-}
replicateM amount x = tightenMonad (G.replicateM amount x)

replicateM' :: (Unbox a, Monad m, IsNat n) => m a -> m (Vec a n)
{-# INLINE replicateM' #-}
replicateM' x = tightenMonad (G.replicateM' x)

generate :: (Unbox a) => SNat n -> (Fin n -> a) -> Vec a n
{-# INLINE generate  #-}
generate amount f = Vec (G.generate amount f)

generate' :: (Unbox a, IsNat n) => (Fin n -> a) -> Vec a n
{-# INLINE generate' #-}
generate' f = Vec (G.generate' f)

generateM :: (Unbox a, Monad m) => SNat n -> (Fin n -> m a) -> m (Vec a n)
{-# INLINE generateM #-}
generateM amount f = tightenMonad (G.generateM amount f)

generateM' :: (Unbox a, Monad m, IsNat n) => (Fin n -> m a) -> m (Vec a n)
{-# INLINE generateM' #-}
generateM' f = tightenMonad (G.generateM' f)

allFin :: SNat n -> Vec (Fin n) n
{-# INLINE allFin #-}
allFin len = Vec (G.allFin len)

allFin' :: (IsNat n) => Vec (Fin n) n
{-# INLINE allFin' #-}
allFin' = Vec (G.allFin')

iterate :: (Unbox a) => SNat n -> (a -> a) -> a -> Vec a n
{-# INLINE iterate #-}
iterate amount f x = Vec (G.iterate amount f x)

iterate' :: (Unbox a, IsNat n) => (a -> a) -> a -> Vec a n
{-# INLINE iterate' #-}
iterate' f x = Vec (G.iterate' f x)

unfoldr :: (Unbox a) => (b -> Maybe (a,b)) -> b -> Sigma (Vec a)
{-# INLINE unfoldr #-}
unfoldr f x = tightenSigma (G.unfoldr f x)

unfoldrN :: (Unbox a) => SNat n -> (b -> Maybe (a,b)) -> b -> Maybe (Vec a n)
{-# INLINE unfoldrN #-}
unfoldrN amount f x = tightenMaybe (G.unfoldrN amount f x)

construct :: (Unbox a) => SNat n -> (forall m. Vec a m -> a) -> Vec a n
{-# INLINE construct #-}
construct amount f = Vec (G.construct amount (f . Vec))

construct' :: (Unbox a, IsNat n) => (forall m. Vec a m -> a) -> Vec a n
{-# INLINE construct' #-}
construct' f = Vec (G.construct' (f . Vec))

constructr :: (Unbox a) => SNat n -> (forall m. Vec a m -> a) -> Vec a n
{-# INLINE constructr #-}
constructr amount f = Vec (G.constructr amount (f . Vec))

constructr' :: (Unbox a, IsNat n) => (forall m. Vec a m -> a) -> Vec a n
{-# INLINE constructr' #-}
constructr' f = Vec (G.constructr' (f . Vec))

enumFromN :: (Unbox a, Num a) => a -> SNat n -> Vec a n
{-# INLINE enumFromN #-}
enumFromN x len = Vec (G.enumFromN x len)

enumFromN' :: (Unbox a, IsNat n, Num a) => a -> Vec a n
{-# INLINE enumFromN' #-}
enumFromN' x = Vec (G.enumFromN' x)

enumFromStepN :: (Unbox a, Num a) => a -> a -> SNat n -> Vec a n
{-# INLINE enumFromStepN #-}
enumFromStepN x y len = Vec (G.enumFromStepN x y len)

enumFromStepN' :: (Unbox a, IsNat n, Num a) => a -> a -> Vec a n
{-# INLINE enumFromStepN' #-}
enumFromStepN' x y = Vec (G.enumFromStepN' x y)

cons :: (Unbox a) => a -> Vec a n -> Vec a ('S n)
{-# INLINE cons #-}
cons x (Vec v) = Vec (G.cons x v)

snoc :: (Unbox a) => Vec a n -> a -> Vec a ('S n)
{-# INLINE snoc #-}
snoc (Vec v) x = Vec (G.snoc v x)

infixr 5 ++
(++) :: (Unbox a) => Vec a n -> Vec a m -> Vec a (n + m)
{-# INLINE (++) #-}
(Vec v) ++ (Vec w) = Vec (v G.++ w)

force :: (Unbox a) => Vec a n -> Vec a n
{-# INLINE force #-}
force (Vec v) = Vec (G.force v)

(//) :: (Unbox a) => Vec a n -> [(Fin n, a)] -> Vec a n
{-# INLINE (//) #-}
(Vec v) // us = Vec (v G.// us)

update :: (Unbox a) => Vec a n -> Vec (Fin n, a) m -> Vec a n
{-# INLINE update #-}
update (Vec v) (Vec w) = Vec (G.update v w)

update_ :: (Unbox a) => Vec a n -> Vec (Fin n) m -> Vec a m -> Vec a n
{-# INLINE update_ #-}
update_ (Vec v) (Vec w) (Vec x) = Vec (G.update_ v w x)

accum :: (Unbox a) => (a -> b -> a) -> Vec a n -> [(Fin n, b)] -> Vec a n
{-# INLINE accum #-}
accum f (Vec v) us = Vec (G.accum f v us)

accumulate :: (Unbox a, Unbox b) => (a -> b -> a) -> Vec a n -> Vec (Fin n, b) m -> Vec a n
{-# INLINE accumulate #-}
accumulate f (Vec v) (Vec w) = Vec (G.accumulate f v w)

accumulate_ :: (Unbox a, Unbox b) => (a -> b -> a) -> Vec a n -> Vec (Fin n) m -> Vec b m -> Vec a n
{-# INLINE accumulate_ #-}
accumulate_ f (Vec v) (Vec w) (Vec x) = Vec (G.accumulate_ f v w x)

reverse :: (Unbox a) => Vec a n -> Vec a n
{-# INLINE reverse #-}
reverse (Vec v) = Vec (G.reverse v)

backpermute :: (Unbox a) => Vec a n -> Vec (Fin n) m -> Vec a m
{-# INLINE backpermute #-}
backpermute (Vec v) (Vec w) = Vec (G.backpermute v w)

indexed :: (Unbox a) => Vec a n -> Vec (Fin n, a) n
{-# INLINE indexed #-}
indexed (Vec v) = Vec (G.indexed v)

map :: (Unbox a, Unbox b) => (a -> b) -> Vec a n -> Vec b n
{-# INLINE map #-}
map f (Vec v) = Vec (G.map f v)

imap :: (Unbox a, Unbox b) => (Fin n -> a -> b) -> Vec a n -> Vec b n
{-# INLINE imap #-}
imap f (Vec v) = Vec (G.imap f v)

concatMap :: (Unbox a, Unbox b) => (a -> Vec b n) -> Vec a m -> Vec b (n * m)
{-# INLINE concatMap #-}
concatMap f (Vec v) = Vec (G.concatMap (getVec . f) v)

mapM :: (Unbox a, Unbox b, Monad m) => (a -> m b) -> Vec a n -> m (Vec b n)
{-# INLINE mapM #-}
mapM f (Vec v) = tightenMonad (G.mapM f v)

imapM :: (Unbox a, Unbox b, Monad m) => (Fin n -> a -> m b) -> Vec a n -> m (Vec b n)
{-# INLINE imapM #-}
imapM f (Vec v) = tightenMonad (G.imapM f v)

mapM_ :: (Unbox a, Monad m) => (a -> m b) -> Vec a n -> m ()
{-# INLINE mapM_ #-}
mapM_ f (Vec v) = G.mapM_ f v

imapM_ :: (Unbox a, Monad m) => (Fin n -> a -> m b) -> Vec a n -> m ()
{-# INLINE imapM_ #-}
imapM_ f (Vec v) = G.imapM_ f v

forM :: (Unbox a, Unbox b, Monad m) => Vec a n -> (a -> m b) -> m (Vec b n)
{-# INLINE forM #-}
forM (Vec v) f = tightenMonad (G.mapM f v)

forM_ :: (Unbox a, Monad m) => Vec a n -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ (Vec v) f = G.mapM_ f v

zipWith :: (Unbox a, Unbox b, Unbox c) =>
  (a -> b -> c) ->
  Vec a n -> Vec b n -> Vec c n
{-# INLINE zipWith #-}
zipWith f (Vec as) (Vec bs) = Vec (G.zipWith f as bs)

zipWith3 :: (Unbox a, Unbox b, Unbox c, Unbox d) =>
  (a -> b -> c -> d) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n
{-# INLINE zipWith3 #-}
zipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.zipWith3 f as bs cs)

zipWith4 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e) =>
  (a -> b -> c -> d -> e) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n
{-# INLINE zipWith4 #-}
zipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zipWith4 f as bs cs ds)

zipWith5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f) =>
  (a -> b -> c -> d -> e -> f) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n -> Vec f n
{-# INLINE zipWith5 #-}
zipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zipWith5 f as bs cs ds es)

zipWith6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g) =>
  (a -> b -> c -> d -> e -> f -> g) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n -> Vec f n -> Vec g n
{-# INLINE zipWith6 #-}
zipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zipWith6 f as bs cs ds es fs)

izipWith :: (Unbox a, Unbox b, Unbox c) =>
  (Fin n -> a -> b -> c) ->
  Vec a n -> Vec b n -> Vec c n
{-# INLINE izipWith #-}
izipWith f (Vec as) (Vec bs) = Vec (G.izipWith f as bs)

izipWith3 :: (Unbox a, Unbox b, Unbox c, Unbox d) =>
  (Fin n -> a -> b -> c -> d) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n
{-# INLINE izipWith3 #-}
izipWith3 f (Vec as) (Vec bs) (Vec cs) = Vec (G.izipWith3 f as bs cs)

izipWith4 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e) =>
  (Fin n -> a -> b -> c -> d -> e) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n
{-# INLINE izipWith4 #-}
izipWith4 f (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.izipWith4 f as bs cs ds)

izipWith5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f) =>
  (Fin n -> a -> b -> c -> d -> e -> f) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n -> Vec f n
{-# INLINE izipWith5 #-}
izipWith5 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.izipWith5 f as bs cs ds es)

izipWith6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f, Unbox g) =>
  (Fin n -> a -> b -> c -> d -> e -> f -> g) ->
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n -> Vec f n -> Vec g n
{-# INLINE izipWith6 #-}
izipWith6 f (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.izipWith6 f as bs cs ds es fs)

zip :: (Unbox a, Unbox b) =>
  Vec a n -> Vec b n -> Vec (a, b) n
{-# INLINE zip #-}
zip (Vec as) (Vec bs) = Vec (G.zip as bs)

zip3 :: (Unbox a, Unbox b, Unbox c) =>
  Vec a n -> Vec b n -> Vec c n -> Vec (a, b, c) n
{-# INLINE zip3 #-}
zip3 (Vec as) (Vec bs) (Vec cs) = Vec (G.zip3 as bs cs)

zip4 :: (Unbox a, Unbox b, Unbox c, Unbox d) =>
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec (a, b, c, d) n
{-# INLINE zip4 #-}
zip4 (Vec as) (Vec bs) (Vec cs) (Vec ds) = Vec (G.zip4 as bs cs ds)

zip5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e) =>
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n -> Vec (a, b, c, d, e) n
{-# INLINE zip5 #-}
zip5 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) = Vec (G.zip5 as bs cs ds es)

zip6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f) =>
  Vec a n -> Vec b n -> Vec c n -> Vec d n -> Vec e n -> Vec f n -> Vec (a, b, c, d, e, f) n
{-# INLINE zip6 #-}
zip6 (Vec as) (Vec bs) (Vec cs) (Vec ds) (Vec es) (Vec fs) = Vec (G.zip6 as bs cs ds es fs)

unzip :: (Unbox a, Unbox b) =>
  Vec (a, b) n -> (Vec a n, Vec b n)
{-# INLINE unzip #-}
unzip (Vec vs) = (Vec as, Vec bs)
  where (as, bs) = G.unzip vs

unzip3 :: (Unbox a, Unbox b, Unbox c) =>
  Vec (a, b, c) n -> (Vec a n, Vec b n, Vec c n)
{-# INLINE unzip3 #-}
unzip3 (Vec vs) = (Vec as, Vec bs, Vec cs)
  where (as, bs, cs) = G.unzip3 vs

unzip4 :: (Unbox a, Unbox b, Unbox c, Unbox d) =>
  Vec (a, b, c, d) n -> (Vec a n, Vec b n, Vec c n, Vec d n)
{-# INLINE unzip4 #-}
unzip4 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds)
  where (as, bs, cs, ds) = G.unzip4 vs

unzip5 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e) =>
  Vec (a, b, c, d, e) n -> (Vec a n, Vec b n, Vec c n, Vec d n, Vec e n)
{-# INLINE unzip5 #-}
unzip5 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds, Vec es)
  where (as, bs, cs, ds, es) = G.unzip5 vs

unzip6 :: (Unbox a, Unbox b, Unbox c, Unbox d, Unbox e, Unbox f) =>
  Vec (a, b, c, d, e, f) n -> (Vec a n, Vec b n, Vec c n, Vec d n, Vec e n, Vec f n)
{-# INLINE unzip6 #-}
unzip6 (Vec vs) = (Vec as, Vec bs, Vec cs, Vec ds, Vec es, Vec fs)
  where (as, bs, cs, ds, es, fs) = G.unzip6 vs

filter :: (Unbox a) => (a -> Bool) -> Vec a n -> Sigma (Vec a)
{-# INLINE filter #-}
filter f (Vec v) = tightenSigma (G.filter f v)

ifilter :: (Unbox a) => (Fin n -> a -> Bool) -> Vec a n -> Sigma (Vec a)
{-# INLINE ifilter #-}
ifilter f (Vec v) = tightenSigma (G.ifilter f v)

filterM :: (Unbox a, Monad m) => (a -> m Bool) -> Vec a n -> m (Sigma (Vec a))
{-# INLINE filterM #-}
filterM f (Vec v) = tightenSigmaM (G.filterM f v)

takeWhile :: (Unbox a) => (a -> Bool) -> Vec a n -> Sigma (Vec a)
{-# INLINE takeWhile #-}
takeWhile f (Vec v) = tightenSigma (G.takeWhile f v)

dropWhile :: (Unbox a) => (a -> Bool) -> Vec a n -> Sigma (Vec a)
{-# INLINE dropWhile #-}
dropWhile f (Vec v) = tightenSigma (G.dropWhile f v)

partition :: (Unbox a) => (a -> Bool) -> Vec a n -> (Sigma (Vec a), Sigma (Vec a))
{-# INLINE partition #-}
partition f (Vec v) = tightenSigma2 (G.partition f v)

unstabelPartition :: (Unbox a) => (a -> Bool) -> Vec a n -> (Sigma (Vec a), Sigma (Vec a))
{-# INLINE unstabelPartition #-}
unstabelPartition f (Vec v) = tightenSigma2 (G.unstablePartition f v)

span :: (Unbox a) => (a -> Bool) -> Vec a n -> (Sigma (Vec a), Sigma (Vec a))
{-# INLINE span #-}
span f (Vec v) = tightenSigma2 (G.span f v)

break :: (Unbox a) => (a -> Bool) -> Vec a n -> (Sigma (Vec a), Sigma (Vec a))
{-# INLINE break #-}
break f (Vec v) = tightenSigma2 (G.break f v)

infix 4 `elem`
elem :: (Unbox a, Eq a) => a -> Vec a n -> Bool
{-# INLINE elem #-}
elem x (Vec v) = G.elem x v

notElem :: (Unbox a, Eq a) => a -> Vec a n -> Bool
{-# INLINE notElem #-}
notElem x (Vec v) = G.notElem x v

find :: (Unbox a) => (a -> Bool) -> Vec a n -> Maybe a
{-# INLINE find #-}
find f (Vec v) = G.find f v

findIndex :: (Unbox a) => (a -> Bool) -> Vec a n -> Maybe (Fin n)
{-# INLINE findIndex #-}
findIndex f (Vec v) = G.findIndex f v

findIndices :: (Unbox a) => (a -> Bool) -> Vec a n -> Sigma (Vec (Fin n))
{-# INLINE findIndices #-}
findIndices f (Vec v) = tightenSigma (G.findIndices f v)

elemIndex :: (Unbox a, Eq a) => a -> Vec a n -> Maybe (Fin n)
{-# INLINE elemIndex #-}
elemIndex x (Vec v) = G.elemIndex x v

elemIndices :: (Unbox a, Eq a) => a -> Vec a n -> Sigma (Vec (Fin n))
{-# INLINE elemIndices #-}
elemIndices x (Vec v) = tightenSigma (G.elemIndices x v)

foldl :: (Unbox b) => (a -> b -> a) -> a -> Vec b n -> a
{-# INLINE foldl #-}
foldl f x (Vec v) = G.foldl f x v

foldl1 :: (Unbox a) => (a -> a -> a) -> Vec a ('S n) -> a
{-# INLINE foldl1 #-}
foldl1 f (Vec v) = G.foldl1 f v

foldl' :: (Unbox b) => (a -> b -> a) -> a -> Vec b n -> a
{-# INLINE foldl' #-}
foldl' f x (Vec v) = G.foldl' f x v

foldl1' :: (Unbox a) => (a -> a -> a) -> Vec a ('S n) -> a
{-# INLINE foldl1' #-}
foldl1' f (Vec v) = G.foldl1' f v

foldlD :: (Unbox b) => (a l -> b -> a ('S l)) -> a 'Z -> Vec b n -> a n
{-# INLINE foldlD #-}
foldlD f x (Vec v) = G.foldlD f x v

foldlD' :: (Unbox b) => (a l -> b -> a ('S l)) -> a 'Z -> Vec b n -> a n
{-# INLINE foldlD' #-}
foldlD' f x (Vec v) = G.foldlD' f x v

foldr :: (Unbox a) => (a -> b -> b) -> b -> Vec a n -> b
{-# INLINE foldr #-}
foldr f x (Vec v) = G.foldr f x v

foldr1 :: (Unbox a) => (a -> a -> a) -> Vec a ('S n) -> a
{-# INLINE foldr1 #-}
foldr1 f (Vec v) = G.foldr1 f v

foldr' :: (Unbox a) => (a -> b -> b) -> b -> Vec a n -> b
{-# INLINE foldr' #-}
foldr' f x (Vec v) = G.foldr' f x v

foldr1' :: (Unbox a) => (a -> a -> a) -> Vec a ('S n) -> a
{-# INLINE foldr1' #-}
foldr1' f (Vec v) = G.foldr1' f v

foldrD :: (Unbox a) => (a -> b l -> b ('S l)) -> b 'Z -> Vec a n -> b n
{-# INLINE foldrD #-}
foldrD f x (Vec v) = G.foldrD f x v

foldrD' :: (Unbox a) => (a -> b l -> b ('S l)) -> b 'Z -> Vec a n -> b n
{-# INLINE foldrD' #-}
foldrD' f x (Vec v) = G.foldrD' f x v

ifoldl :: (Unbox b) => (a -> Fin n -> b -> a) -> a -> Vec b n -> a
{-# INLINE ifoldl #-}
ifoldl f x (Vec v) = G.ifoldl f x v

ifoldl' :: (Unbox b) => (a -> Fin n -> b -> a) -> a -> Vec b n -> a
{-# INLINE ifoldl' #-}
ifoldl' f x (Vec v) = G.ifoldl' f x v

ifoldlD :: (Unbox b) => (a l -> Fin n -> b -> a ('S l)) -> a 'Z -> Vec b n -> a n
{-# INLINE ifoldlD #-}
ifoldlD f x (Vec v) = G.ifoldlD f x v

ifoldlD' :: (Unbox b) => (a l -> Fin n -> b -> a ('S l)) -> a 'Z -> Vec b n -> a n
{-# INLINE ifoldlD' #-}
ifoldlD' f x (Vec v) = G.ifoldlD' f x v

ifoldr :: (Unbox a) => (Fin n -> a -> b -> b) -> b -> Vec a n -> b
{-# INLINE ifoldr #-}
ifoldr f x (Vec v) = G.ifoldr f x v

ifoldr' :: (Unbox a) => (Fin n -> a -> b -> b) -> b -> Vec a n -> b
{-# INLINE ifoldr' #-}
ifoldr' f x (Vec v) = G.ifoldr' f x v

ifoldrD :: (Unbox a) => (Fin n -> a -> b l -> b ('S l)) -> b 'Z -> Vec a n -> b n
{-# INLINE ifoldrD #-}
ifoldrD f x (Vec v) = G.ifoldrD f x v

ifoldrD' :: (Unbox a) => (Fin n -> a -> b l -> b ('S l)) -> b 'Z -> Vec a n -> b n
{-# INLINE ifoldrD' #-}
ifoldrD' f x (Vec v) = G.ifoldrD' f x v

all :: (Unbox a) => (a -> Bool) -> Vec a n -> Bool
{-# INLINE all #-}
all p (Vec v) = G.all p v

any :: (Unbox a) => (a -> Bool) -> Vec a n -> Bool
{-# INLINE any #-}
any p (Vec v) = G.any p v

and ::  Vec Bool n -> Bool
{-# INLINE and #-}
and (Vec v) = G.and v

or ::  Vec Bool n -> Bool
{-# INLINE or #-}
or (Vec v) = G.or v

sum :: (Unbox a, Num a) => Vec a n -> a
{-# INLINE sum #-}
sum (Vec v) = G.sum v

product :: (Unbox a, Num a) => Vec a n -> a
{-# INLINE product #-}
product (Vec v) = G.product v

maximum :: (Unbox a, Ord a) => Vec a ('S n) -> a
{-# INLINE maximum #-}
maximum (Vec v) = G.maximum v

maximumBy :: (Unbox a) => (a -> a -> Ordering) -> Vec a ('S n) -> a
{-# INLINE maximumBy #-}
maximumBy c (Vec v) = G.maximumBy c v

minimum :: (Unbox a, Ord a) => Vec a ('S n) -> a
{-# INLINE minimum #-}
minimum (Vec v) = G.minimum v

minimumBy :: (Unbox a) => (a -> a -> Ordering) -> Vec a ('S n) -> a
{-# INLINE minimumBy #-}
minimumBy c (Vec v) = G.minimumBy c v

minIndex :: (Unbox a, Ord a) => Vec a ('S n) -> Fin ('S n)
{-# INLINE minIndex #-}
minIndex (Vec v) = G.minIndex v

minIndexBy :: (Unbox a) => (a -> a -> Ordering) -> Vec a ('S n) -> Fin ('S n)
{-# INLINE minIndexBy #-}
minIndexBy c (Vec v) = G.minIndexBy c v

maxIndex :: (Unbox a, Ord a) => Vec a ('S n) -> Fin ('S n)
{-# INLINE maxIndex #-}
maxIndex (Vec v) = G.maxIndex v

maxIndexBy :: (Unbox a) => (a -> a -> Ordering) -> Vec a ('S n) -> Fin ('S n)
{-# INLINE maxIndexBy #-}
maxIndexBy c (Vec v) = G.maxIndexBy c v

toList :: (Unbox a) => Vec a n -> [a]
{-# INLINE toList #-}
toList (Vec v) = G.toList v

fromList :: (Unbox a) => [a] -> Sigma (Vec a)
{-# INLINE fromList #-}
fromList list = tightenSigma (G.fromList list)

fromListN :: (Unbox a) => SNat n -> [a] -> Maybe (Vec a n)
{-# INLINE fromListN #-}
fromListN len list = tightenMaybe (G.fromListN len list)

fromListN' :: (Unbox a, IsNat n) => [a] -> Maybe (Vec a n)
{-# INLINE fromListN' #-}
fromListN' list = tightenMaybe (G.fromListN' list)

toVector :: Vec a n -> U.Vector a
{-# INLINE toVector #-}
toVector (Vec v) = G.toVector v

fromVector :: (Unbox a) => U.Vector a -> Sigma (Vec a)
{-# INLINE fromVector #-}
fromVector v = tightenSigma (G.fromVector v)

fromVectorN :: (Unbox a) => SNat n -> U.Vector a -> Maybe (Vec a n)
{-# INLINE fromVectorN #-}
fromVectorN len v = coerce (G.fromVectorN len v)

fromVectorN' :: (Unbox a, IsNat n) => U.Vector a -> Maybe (Vec a n)
{-# INLINE fromVectorN' #-}
fromVectorN' v = tightenMaybe (G.fromVectorN' v)

convert :: (GV.Vector v a, GV.Vector w a) => G.Vec v a n -> G.Vec w a n
{-# INLINE convert #-}
convert v = G.convert v


