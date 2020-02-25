{-# OPTIONS_GHC -Wall -fdefer-type-errors #-}
module Data.PMA.Mutable where

import           Control.Monad.Primitive
import           Data.Vector.Mutable     (MVector)
import qualified Data.Vector.Mutable     as MVector

data MPMA s a = MPMA { getMPMA :: MVector s a }

insert :: PrimMonad m => MPMA (PrimState m) a -> Int -> a -> m ()
insert = _

insertBatch :: PrimMonad m => [a] -> MPMA (PrimState m) a -> m ()
insertBatch = _
