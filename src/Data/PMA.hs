{-# OPTIONS_GHC -Wall -fdefer-type-errors #-}
module Data.PMA where

import           Control.Monad.Primitive
import           Control.Monad.ST        (runST)

import           Data.Vector             (Vector)
import qualified Data.Vector             as Vector

import           Data.PMA.Mutable        (MPMA)
import qualified Data.PMA.Mutable        as MPMA

data PMA a = PMA { getPMA :: Vector a }

freeze :: PrimMonad m => MPMA (PrimState m) a -> m (PMA a)
freeze = _

thaw :: PrimMonad m => PMA a -> m (MPMA (PrimState m) a)
thaw = _

{-# RULES
   "insertBatch/insertBatch"  forall xs ys. insertBatch xs . insertBatch ys = insertBatch (xs ++ ys)
#-}

insertBatch :: [a] -> PMA a -> PMA a
insertBatch values pma = runST $ do
  mpma <- thaw pma
  MPMA.insertBatch values mpma
  freeze mpma
