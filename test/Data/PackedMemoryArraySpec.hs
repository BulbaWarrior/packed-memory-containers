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
      it "is always sorted" $ 
        property $ prop_sorted emptyPMA

      it "'s total cardinality is always equal to its segment cardinalities sum" $
        property $ prop_cardinalities emptyPMA

      it "'s nonempy slots counts should be equal to its cardinality" $
        property $ prop_elements_count emptyPMA


-- | Repsents a possible operations over a PMA
data Operation a = Insert a 
  deriving (Eq, Show)

-- | Applies given operation to given PMA
apply :: (Ord a, Show a) => Operation a -> PMA a -> PMA a
apply (Insert a) pma = insert pma a

-- | Applies all the operations from list to given PMA
-- from right to left
construct :: (Ord a, Show a) => PMA a -> [Operation a] -> PMA a
construct = foldr apply 


-- | Generates an Insert Opertion.
instance (Integral a) => Arbitrary (Operation a) where
  arbitrary = do
    x <- choose(-1000, 1000) :: Gen Int

    return $ Insert (fromIntegral x)



-- | Applies given operations to given PMA
-- and then checks its elements for being sorted.
prop_sorted :: (Integral a, Show a, Ord a) 
                => PMA a -> [Operation a] -> Bool
prop_sorted initial operations = sort asList == asList
  where
    pma = construct initial operations
    asList = catMaybes (Vector.toList (elements pma))


-- | Applies given operations to given PMA
-- and then checks its cardinalities
prop_cardinalities :: (Integral a, Show a, Ord a)
                      => PMA a -> [Operation a] -> Bool
prop_cardinalities initial operations = card == sum cards
    where
      pma = construct initial operations
      card = cardinality pma
      cards = segmentsCardinalities pma


prop_elements_count :: (Integral a, Show a, Ord a)
                    => PMA a -> [Operation a] -> Bool
prop_elements_count initial operations = count == length filteredElems
    where
      pma = construct initial operations
      filteredElems = Vector.filter isJust (elements pma)
      count = cardinality pma
