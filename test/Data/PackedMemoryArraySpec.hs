{-# LANGUAGE FlexibleInstances #-}
module Data.PackedMemoryArraySpec where

import           Data.PackedMemoryArray

import           Test.Hspec
import           Test.QuickCheck hiding (elements)

import qualified Data.Vector as Vector
import           Data.List (sort)
import           Data.Maybe (catMaybes, isJust)


spec :: Spec
spec 
  = do
    describe "PMA" $ do
      it "is always sorted"
        $ property $ prop_sorted empty

      it "'s total cardinality is always equal to its segment cardinalities sum"
        $ property $ prop_totalSegmentCardinalities empty

      it "'s nonempy slots counts should be equal to its cardinality"
        $ property $ prop_elementsCount empty


-- | Repsents a possible operations over a PMA
data Operation k a = Insert k a
  deriving (Eq, Show)

-- | Applies given operation to given PMA
apply :: (Ord k) => Operation k a -> PMA k a -> PMA k a
apply (Insert k a) pma = insert k a pma

-- | Applies all the operations from list to given PMA
-- from right to left
construct :: (Ord k) => PMA k a -> [Operation k a] -> PMA k a
construct = foldr apply


-- | Generates an Insert Opertion.
instance (Integral k, Show k) => Arbitrary (Operation k String) where
  arbitrary = do
    x <- choose(-1000, 1000) :: Gen Int

    return $ Insert (fromIntegral x) (show x)


-- | Applies given operations to given PMA
-- and then checks its elements for being sorted.
prop_sorted :: (Integral k, Show k, Ord k) 
                => PMA k String -> [Operation k String] -> Bool
prop_sorted initial operations = sort keys == keys
  where
    pma = construct initial operations
    asList = catMaybes (Vector.toList (elements pma))
    keys = fmap fst asList


-- | Applies given operations to given PMA
-- and then checks if segments cardinalities 
-- add up to total cardinality
prop_totalSegmentCardinalities :: (Integral k, Show k, Ord k)
                      => PMA k String -> [Operation k String] -> Bool
prop_totalSegmentCardinalities initial operations = card == sum cards
  where
    pma = construct initial operations
    card = cardinality pma
    cards = segmentsCardinalities pma


-- | Applies given operations to given PMA
-- and then checks if total cardinality
-- is equal to actual elements number inside the pma
prop_elementsCount :: (Integral k, Show k, Ord k)
                    => PMA k String -> [Operation k String] -> Bool
prop_elementsCount initial operations = count == length filteredElems
  where
    pma = construct initial operations
    filteredElems = Vector.filter isJust (elements pma)
    count = cardinality pma
