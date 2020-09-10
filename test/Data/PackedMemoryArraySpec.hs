module Data.PackedMemoryArraySpec where

import Data.PackedMemoryArray
import Test.Hspec
import Test.QuickCheck hiding (elements)
import Data.Vector (toList)
import Data.List (sort)
import Data.Maybe (catMaybes)


spec :: Spec
spec 
  = do
    describe "PMA" $ do
      it "is always sorted" $ 
        property $ prop_sorted emptyPMA


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
    asList = catMaybes (toList (elements pma))
