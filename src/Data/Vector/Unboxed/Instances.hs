{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Vector.Unboxed.Instances where

import Control.Monad (liftM)

import Data.Nat.Internal (Fin(..))
import Data.Vector.Unboxed (Unbox,Vector,MVector)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M

newtype instance MVector s (Fin n) = MV_Fin (MVector s Int)
newtype instance Vector    (Fin n) = V_Fin  (Vector    Int)

instance G.Vector Vector (Fin n) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Fin v) = V_Fin `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Fin v) = MV_Fin `liftM` G.basicUnsafeThaw v
  basicLength (V_Fin v) = G.basicLength v
  basicUnsafeSlice i n (V_Fin v) = V_Fin $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Fin v) i
                = Fin `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Fin mv) (V_Fin v)
                = G.basicUnsafeCopy mv v
  elemseq _ (Fin x) z = G.elemseq (undefined :: Vector Int) x z

instance M.MVector MVector (Fin n) where
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
  basicLength (MV_Fin v) = M.basicLength v
  basicUnsafeSlice i n (MV_Fin v) = MV_Fin $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Fin v1) (MV_Fin v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Fin `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Fin v) = M.basicInitialize v
  basicUnsafeReplicate n (Fin x) = MV_Fin `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_Fin v) i = Fin `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Fin v) i (Fin x) = M.basicUnsafeWrite v i x
  basicClear (MV_Fin v) = M.basicClear v
  basicSet (MV_Fin v) (Fin x) = M.basicSet v x
  basicUnsafeCopy (MV_Fin v1) (MV_Fin v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Fin v1) (MV_Fin v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Fin v) n = MV_Fin `liftM` M.basicUnsafeGrow v n

instance Unbox (Fin n)
