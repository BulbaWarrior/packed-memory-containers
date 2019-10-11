{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module Data.PackedMemoryQuadtree where

import           Data.Char    (chr, ord)
import           Data.Ord     (comparing)
import           Data.Vector  (Vector)
import qualified Data.Vector  as Vector
import           GHC.TypeLits (KnownNat, Nat)
import           Numeric      (readInt, showIntAtBase)

data Coords (n :: Nat) = Coords Int Int

-- | Packed-memory (sorted) array of values.
data PMA a = PMA
  { pmaValues :: Vector (Maybe a)
  , pmaValid  :: !Int
  }

emptyPMA :: PMA a
emptyPMA = PMA
  { pmaValues = Vector.empty
  , pmaValid  = 0
  }




newtype ZIndex (n :: Nat) = ZIndex Int
  deriving (Eq, Ord, Show)

data ZIndexed n a = ZIndexed
  { zindexedIndex :: ZIndex n
  , zindexedValue :: a
  }

instance Eq (ZIndexed n a) where x == y = compare x y == EQ
instance Ord (ZIndexed n a) where compare = comparing zindexedIndex

showBinary :: Int -> String
showBinary n = showIntAtBase 2 (\d -> chr (d + ord '0')) n ""

readBinary :: String -> Int
readBinary = fst . head . readInt 2 (const True) (\c -> ord c - ord '0')

interleaveBinary :: String -> String -> String
interleaveBinary xs ys = concat (alignWith (\x y -> x : y : "") xs ys)
  where
    alignWith f [] ys         = map (f '0') ys
    alignWith f xs []         = map (`f` '0') xs
    alignWith f (x:xs) (y:ys) = f x y : alignWith f xs ys

-- |
-- >>> zindexOf (Coords @23 1 0)
-- ZIndex 1
-- >>> zindexOf (Coords @23 0 1)
-- ZIndex 2
-- >>> zindexOf (Coords @23 1 1)
-- ZIndex 3
-- >>> zindexOf (Coords @23 12 23)
-- ZIndex 634
zindexOf :: KnownNat n => Coords n -> ZIndex n
zindexOf (Coords x y) = ZIndex $
  readBinary (reverse (interleaveBinary (reverse (showBinary x)) (reverse (showBinary y))))



data PMQ n t a = PMQ
  { pmqSegments :: PMA (ZIndexed n [(t, a)])
  , pmqCounts   :: Vector Int
  }

emptyPMQ :: PMQ n t a
emptyPMQ = PMQ
  { pmqSegments = emptyPMA
  , pmqCounts = Vector.empty
  }

rebalancePMQ :: PMQ n t a -> PMQ n t a
rebalancePMQ = undefined

insert :: (Coords n, a) -> PMQ n t a -> PMQ n t a
insert = undefined

insertBatch :: [(Coords n, a)] -> PMQ n t a -> PMQ n t a
insertBatch = undefined

