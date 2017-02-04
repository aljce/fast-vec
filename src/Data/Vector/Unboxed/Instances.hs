{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Vector.Unboxed.Instances where

import Control.Monad (liftM)

import Data.Nat.Internal (Bound(..))
import Data.Vector.Unboxed (Unbox,Vector,MVector)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M

newtype instance MVector s (Bound n) = MV_Bound (MVector s Int)
newtype instance Vector    (Bound n) = V_Bound  (Vector    Int)

instance G.Vector Vector (Bound n) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Bound v) = V_Bound `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Bound v) = MV_Bound `liftM` G.basicUnsafeThaw v
  basicLength (V_Bound v) = G.basicLength v
  basicUnsafeSlice i n (V_Bound v) = V_Bound $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Bound v) i
                = Bound `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Bound mv) (V_Bound v)
                = G.basicUnsafeCopy mv v
  elemseq _ (Bound x) z = G.elemseq (undefined :: Vector Int) x z

instance M.MVector MVector (Bound n) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Bound v) = M.basicLength v
  basicUnsafeSlice i n (MV_Bound v) = MV_Bound $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Bound v1) (MV_Bound v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Bound `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Bound v) = M.basicInitialize v
  basicUnsafeReplicate n (Bound x) = MV_Bound `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Bound v) i = Bound `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Bound v) i (Bound x) = M.basicUnsafeWrite v i x
  basicClear (MV_Bound v) = M.basicClear v
  basicSet (MV_Bound v) (Bound x) = M.basicSet v x
  basicUnsafeCopy (MV_Bound v1) (MV_Bound v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Bound v1) (MV_Bound v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Bound v) n = MV_Bound `liftM` M.basicUnsafeGrow v n

instance Unbox (Bound n)
