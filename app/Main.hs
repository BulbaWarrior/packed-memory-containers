import qualified Data.PMAMap as PMC
import qualified Data.PackedMemoryArray as PMA
import qualified Data.PMA as NewPMA
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Control.Monad.ST

insert :: (Ord k) => k -> a -> PMC.Map k a -> PMC.Map k a
insert = PMC.insertE

myMap :: [Int] -> PMC.Map Int Int
myMap = foldr (\x map -> insert x x map) PMC.empty

myPma :: [Int] -> PMA.PMA Int Int
-- myPma xs = PMA.fromList $ zip xs xs
myPma = Vector.foldl (\pma x -> PMA.insert x x pma) PMA.empty . Vector.fromList

newPma :: [Int] -> NewPMA.PMA Int Int
newPma = NewPMA.fromList . map (\x -> (x, x))

-- generate :: (Foldable f) => (k -> v) -> f k -> PMC.Map k v
-- generate f fold = foldr (\k map -> insert k (f k) map) PMC.empty
-- {-# INLINE generate #-}
-- {-# RULES "lookup/generate" PMC.lookup k (generate f vs) = f k #-}

-- currentMap :: (Ord k) => [(k, v)] -> PMC.Map k v
-- currentMap = foldr (\(k, v) map -> insert k v map) PMC.empty

-- desiredMap :: (Ord k) => [(k, v)] -> PMC.Map k v
-- desiredMap assocs map = runST $ do
--   pmc <- PMC.thaw map
--   PMC.batchInsert assocs
--   res <- PMC.freeze pmc
--   return res

myVector :: [Int] -> Vector.Vector Int
myVector = foldr (\x vec -> vec <> Vector.singleton x) Vector.empty
-- myVector = Vector.fromList
-- pmaExample :: Maybe Int
-- pmaExample = do
--   let m = myMap [1..10^3]
--   v1 <- PMC.lookup 7 m
--   v2 <- PMC.lookup 10 $ foldr (\x map -> insert x x map) m
--   return (v1 + v2)

vectorReplace :: Vector.Vector a -> Int -> a -> Vector.Vector a
vectorReplace vec ind val = runST $ do
  v <- Vector.thaw vec
  MVector.write v ind val
  res <- Vector.freeze v
  return res



pmcDemo = do
  let Just res = PMC.lookup 123 $ myMap [1..10^5]
  print res

pmaDemo = do
  let res = PMA.lookup 123 $ myPma [1..2*(10^4)]
  print res

vectorDemo = do
  let res = myVector [1..10^5] Vector.! 123
  print res

newPmaDemo = do
  let Just res = NewPMA.lookup 123 $ newPma [1..(10^7)]
  print res
main = vectorDemo
