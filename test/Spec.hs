import Data.PackedMemoryArray
import Test.QuickCheck
import Data.Vector (toList)
import Data.List (sort, tails)
import Data.Maybe (isJust, fromJust)


main :: IO ()
main = quickCheck (prop_sorted emptyPMA)


-- | Repsents a possible operations over a PMA
data Operation a = Insert a deriving (Eq, Show)

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
    x <- choose(1, 1000) :: Gen Int

    return $ Insert (fromIntegral x)



-- | Applies given operations to given PMA
-- and then checks its elements for being sorted.
prop_sorted :: (Integral a, Show a, Ord a) 
                => PMA a -> [Operation a] -> Bool
prop_sorted initial ops = sort elems == elems
  where
    asList = toList (Data.PackedMemoryArray.elements (construct initial ops))
    elems = map fromJust (filter isJust asList)
