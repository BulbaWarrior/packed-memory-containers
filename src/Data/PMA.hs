{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.PMA where

import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Applicative

import           Data.Vector             -- (Vector)
import qualified Data.Vector             as Vector

import           Data.Vector.Mutable     -- (MVector)
import qualified Data.Vector.Mutable     as MVector


import           Data.Maybe (isJust, isNothing, fromJust, fromMaybe, catMaybes)

import Debug.Trace

log2 :: Int -> Double
log2 = logBase 2 . fromIntegral

hyperceil :: Int -> Int
hyperceil n = 2 ^ (ceiling . log2 $ n)

ceil_div :: Int -> Int -> Int
x `ceil_div` y = (1 + ((x-1) `div` y))
-- Upper density thresholds
t_h :: Double -- root
t_h = 0.75

t_0 :: Double -- leaves
t_0 = 1.00

-- Lower density thresholds
p_h :: Double -- root
p_h = 0.50

p_0 :: Double -- leaves
p_0 = 0.25

max_sparseness :: Int
max_sparseness = ceiling $ 1 / p_0

largest_empty_segment :: Int
largest_empty_segment = 1 * max_sparseness

data PMA k v = PMA
  { cardinality :: Int
  , segmentSize :: Int
  , height :: Int
  , cells :: Vector (Maybe (k, v))
  } deriving (Show)

capacity :: PMA k v -> Int
capacity pma = Vector.length . cells $ pma

segmentCount :: PMA k v -> Int
segmentCount pma = capacity pma `div` segmentSize pma

getElements :: PMA k v -> Vector (k, v)
getElements = Vector.map (\(Just x) -> x) . Vector.filter isJust . cells

-- backwards compatibility with PMC
elements :: PMA k v -> Vector (Maybe (k, v))
elements = Vector.filter isJust . cells

delta_t :: PMA k v -> Double
delta_t pma = (t_0 - t_h) / (fromIntegral . height $ pma)

delta_p :: PMA k v -> Double
delta_p pma = (p_0 - p_h) / (fromIntegral . height $ pma)

minCapacity :: Int
minCapacity = 8

-- type SegmentIndex = Int
-- firstElement :: Vector (Maybe (k, v)) -> SegmentIndex
-- findSegment :: k -> PMA k a -> Int
-- findSegment k pma = binsearch 0 (capacity pma)
--   where
--     binsearch left right
--       | left == right = left
--       | otherwise = do
    
empty :: PMA k a
empty = PMA
  { cardinality = 0
  , segmentSize
  , height = (floor . log2 $ segCount) + 1
  , cells = Vector.replicate capacity Nothing
  }
  where
    segmentSize = 1
    capacity = 2^segmentSize
    segCount = capacity `div` segmentSize

null :: PMA k a -> Bool
null pma = cardinality pma == 0

findl :: Int -> (a -> Bool) -> Vector a -> Maybe Int
findl (-1) _ _ = Nothing
findl pos p vec
  | p $ vec Vector.! pos = Just pos
  | otherwise = findl (pos-1) p vec

findr :: Int -> (a -> Bool) -> Vector a -> Maybe Int
findr pos p vec
  | pos >= Vector.length vec = Nothing
  | p $ vec Vector.! pos = Just pos
  | otherwise = findr (pos+1) p vec

-- search left for the first non-empty cell
searchLeft :: Vector (Maybe a) -> Int -> Int
searchLeft _ (-1) = -1
searchLeft vec pos
  | isJust (vec Vector.! pos) = pos
  | otherwise = searchLeft vec (pos-1)


binsearch :: (Ord k) => k -> Vector (Maybe (k, a)) -> Int
binsearch k vec = binsearch' k vec 0 (Vector.length vec - 1)
binsearch' :: (Ord k) => k -> Vector (Maybe (k, a)) -> Int -> Int -> Int
binsearch' k vec from to
  | key result == Just k = searchLeft vec mid
  | from >= to = predecessor
  | key result > Just k = binsearch' k vec from (mid-1)
  | otherwise = binsearch' k vec (mid+1) to
  where
    mid = from + (to - from) `div` 2
    result = case searchLeft vec mid of
      (-1) -> Nothing
      n -> vec Vector.! n
    key :: Maybe (k, a) -> Maybe k
    key = fmap fst
    predecessor = if key result > Just k
      then searchLeft vec (mid-1)
      else searchLeft vec mid

lookup :: (Ord k) => k -> PMA k a -> Maybe a
lookup k pma
  | key result == Just k = value result
  | otherwise = Nothing
  where
    pos = binsearch k (cells pma)
    result = if pos >= 0
             then cells pma Vector.! pos
             else Nothing
    key = fmap fst
    value = fmap snd

getCell :: Int -> PMA k a -> Maybe (k, a)
getCell pos pma = (cells pma) Vector.! pos

-- | convert a sorted vector into pma
fromVector :: (Ord k) => Vector (k, a) -> PMA k a
fromVector vec = runST $ do
  let metaData = PMA { cardinality = Vector.length vec }
  cells <- Vector.thaw . Vector.map Just $ vec
  resize metaData cells

fromList :: (Ord k) => [(k, a)] -> PMA k a
fromList = fromVector . Vector.fromList


insert :: (Ord k) => k -> a -> PMA k a -> PMA k a
insert k v pma
  | pos < 0 = updatedPma
  | fmap fst (cells (pma) Vector.! pos) == Just k = updatedInPlace
  | otherwise = updatedPma
  where
    pos = binsearch k (cells pma)
    updatedInPlace = pma
      { cells = Vector.modify (\vec -> write vec pos $ Just (k,v)) $ cells pma }

    updatedPma = runST $ do
      let newPma = pma {cardinality = cardinality pma + 1}
      mcells <- Vector.thaw (cells pma)
      insertPos <- insertAfter pos (k,v) mcells
      balancedPma <- rebalance newPma insertPos mcells
      return balancedPma

    -- updatedPma =
    --   let newPma = pma {cardinality = cardinality pma + 1}
    --       newCells =
    --         Vector.modify
    --           (\mcells -> runST $ do
    --               insertPos <- insertAfter pos (k,v) mcells

    --           )


insertAfter :: Int -> a -> MVector s (Maybe a) -> ST s Int
insertAfter pos a vec = do
  insertPos <- shiftElems (pos+1) vec
  MVector.write vec insertPos (Just a)
  return insertPos


shiftElems
  :: Int
  -> MVector s (Maybe a)
  -> ST s Int
shiftElems pos vec
  | pos >= MVector.length vec = do
      _ <- shiftElems (MVector.length vec - 1) vec
      return $ MVector.length vec - 1
  | otherwise = do
      cell <- MVector.read vec pos
      if isNothing cell
        then return pos
        else do
          fvec <- freeze vec
          let emptyPos = findShift fvec pos
          if emptyPos > pos
            then shiftRight pos emptyPos
            else shiftLeft pos emptyPos
  where
    shiftRight pos emptyPos = do
      let sliceLen = emptyPos - pos
          fromSlice = MVector.slice pos sliceLen vec
          toSlice = MVector.slice (pos+1) sliceLen vec
      move toSlice fromSlice
      return pos

    shiftLeft pos emptyPos = do
      let sliceLen = pos - emptyPos
          fromSlice = MVector.slice (emptyPos+1) sliceLen vec
          toSlice = MVector.slice emptyPos sliceLen vec
      MVector.move toSlice fromSlice
      return (pos-1)


-- search right for the first *empty* cell, if there isn't one, search left
findShift :: Vector (Maybe a) -> Int -> Int
findShift vec pos =
  fromJust ((pos +) <$> findIndex isNothing right
  <|> findLastIndex isNothing left)
  where
    (left, right) = Vector.splitAt pos vec

findLastIndex :: (a -> Bool) -> Vector a -> Maybe Int
findLastIndex p = ifoldr' (\i x acc -> acc <|> if p x then Just i else Nothing) Nothing


-- | Shift the cells in PMA vector,
-- preparing the insertion at position @pos@.
-- In the process, the valid position to insert may change.
-- Resulting vector will have some cells shifted.
-- An updated position to be used for insertion is returned.
--
-- >>> positive x = if x <= 0 then Nothing else Just x
-- >>> fromPositive = fmap (fromMaybe 0)
-- >>> v = positive <$> [0, 0, 1, 3, 5, 7, 11, 0, 0, 15, 18, 20] :: V.Vector (Maybe Int)
-- >>> first fromPositive $ runST $ shiftCells 4 v
-- ([0,0,1,3,5,5,7,11,0,15,18,20],4)
-- >>> first fromPositive $ runST $ shiftCells 9 v
--([0,0,1,3,5,7,11,0,15,15,18,20],8)
shiftCells
  :: Int                -- ^ Position in the PMA that we need to free?.
  -> Vector (Maybe a)   -- ^ Vector, representing PMA.
  -> ST s (Vector (Maybe a), Int)
shiftCells pos vec
  | pos >= Vector.length vec = do
      (newVec, _) <- shiftCells (pos - 1) vec
      return (newVec, pos - 1)
  | isJust (vec Vector.! pos) = shiftCells' pos emptyPos vec
  | otherwise = return (vec, pos)
  where emptyPos = findShift vec pos
shiftCells' :: Int -> Int -> Vector (Maybe a) -> ST s ((Vector (Maybe a), Int))
shiftCells' pos emptyPos
  | pos < emptyPos = shiftCellsR pos emptyPos
  | otherwise = shiftCellsL pos emptyPos

-- shift cells from position to the first empty cell to the right
shiftCellsR :: Int -> Int -> Vector (Maybe a) -> ST s ((Vector (Maybe a), Int))
shiftCellsR pos emptyPos vec = do
  v <- Vector.thaw vec
  let sliceLen = emptyPos - pos
  let fromSlice = MVector.slice pos sliceLen v
  let toSlice = MVector.slice (pos+1) sliceLen v
  MVector.move toSlice fromSlice
  newVec <- Vector.freeze v
  return (newVec, pos)

-- shift cells from position to the first empty cell to the left
shiftCellsL :: Int -> Int -> Vector (Maybe a) -> ST s ((Vector (Maybe a), Int))
shiftCellsL pos emptyPos vec = do
    v <- Vector.thaw vec
    let sliceLen = pos - emptyPos
    let fromSlice = MVector.slice (emptyPos+1) sliceLen v
    let toSlice = MVector.slice emptyPos sliceLen v
    MVector.move toSlice fromSlice
    newVec <- Vector.freeze v
    return (newVec, pos-1)

-- | find a minimal window that satisfies density constraints and
-- contains the given position, if such window exists
findWindow
  :: PMA k a -- ^ pma to take metadata from
  -> Int     -- ^ position in the vector that has to be in the window
  -> MVector s (Maybe (k,a)) -- ^ new cells vector of the pma
  -> ST s (Maybe (MVector s (Maybe (k,a))))
findWindow pma pos cells = window' pma pos 0 cells

window'
  :: PMA k a -- ^ pma to take metadata from
  -> Int     -- ^ position in the vector that has to be in the window
  -> Int     -- ^ current attempted height of the window
  -> MVector s (Maybe (k,a)) -- ^ new cells vector of the pma
  -> ST s (Maybe (MVector s (Maybe (k,a))))
window' pma pos h cells
  | h < height pma = do
      fcandidate <- Vector.freeze candidate
      let density = (fromIntegral . occupancy) fcandidate / fromIntegral windowSize
      if validWindow density
        then return (Just candidate)
        else window' pma pos (h+1) cells
  | otherwise = return Nothing
  where
    windowSize = (segmentSize pma) * 2^h
    windowId = pos `div` windowSize
    windowStart = windowId * windowSize
    candidate = MVector.slice windowStart windowSize cells
    t_height = t_0 - (fromIntegral h * delta_t pma)
    p_height = p_0 + (fromIntegral h * delta_p pma)
    validWindow density = p_height <= density && density < t_height


occupancy :: Vector (Maybe a) -> Int
occupancy = Vector.length . Vector.filter isJust

rebalance :: PMA k a -> Int -> MVector s (Maybe (k,a)) -> ST s (PMA k a)
rebalance pma pos vec = do
  maybeWindow <- findWindow pma pos vec
  case maybeWindow of
    Just window -> do
      spread window
      newCells <- Vector.freeze vec
      return pma {cells = newCells}
    Nothing -> do
      newPma <- resize pma vec
      return newPma

resize :: PMA k a -> MVector s (Maybe (k, a)) -> ST s (PMA k a)
resize pma cells = do
  let idealSegSize = ceiling . logBase 2 . fromIntegral $ cardinality pma
      idealNumSegments = cardinality pma `ceil_div` idealSegSize
      newNumSegments = hyperceil idealNumSegments -- has to be power of 2
      newSegSize = (cardinality pma `ceil_div` newNumSegments) * max_sparseness
      newCapacity = newSegSize * newNumSegments
      newHeight = (floor . logBase 2 . fromIntegral $ newNumSegments) + 1
      capacityDelta = (newCapacity - MVector.length cells)
  newCells <- MVector.grow cells capacityDelta
  empties <- MVector.replicate capacityDelta Nothing
  MVector.move (MVector.slice (MVector.length cells) capacityDelta newCells) empties
  spread newCells
  newCells <- Vector.freeze newCells
  return $ PMA
    { cardinality = cardinality pma
    , segmentSize = newSegSize
    , height = newHeight
    , cells = newCells
    }


-- rebalance :: Int -> PMA k a -> PMA k a
-- rebalance pos pma =
--   case window pma pos (cells pma) of
--     Just slice -> spread pma . pack pma $ slice
--     Nothing -> resize pma

-- pack :: MVector s (Maybe a) -> ST s ()
-- pack window = do
--   vec <- freeze window
--   let elems = Vector.filter isJust $ vec
--   melems <- thaw elems
--   empty <- MVector.replicate (MVector.length window - MVector.length melems) Nothing
--   let (windowElems, windowEmpty) = MVector.splitAt (MVector.length melems) window
--   move windowElems melems
--   move windowEmpty empty

spread :: MVector s (Maybe a) -> ST s ()
spread window = do
  fWindow <- freeze window
  let elems = Vector.filter isJust fWindow
      occupancy = Vector.length elems
      capacity = MVector.length window
      frequency = fromIntegral capacity / fromIntegral occupancy
  spread' elems window frequency 0

spread' :: Vector (Maybe a) -> MVector s (Maybe a) -> Double -> Double -> ST s ()
spread' elems cells frequency totalElems
  | totalElems >= fromIntegral (MVector.length cells) = return ()
  | otherwise = do
      let startIndex = floor totalElems -- included
          endIndex = if (totalElems + frequency) < fromIntegral (MVector.length cells)
            then floor (totalElems + frequency) -- excluded
            else (MVector.length cells)
          insertNum = endIndex - startIndex
          insertSlice = MVector.slice startIndex insertNum cells
      if (Vector.length elems > 0)
        then do
          write insertSlice 0 (Vector.head elems)
          empties <- MVector.replicate (insertNum-1) Nothing
          move (MVector.tail insertSlice) empties
        else do
          empties <- MVector.replicate insertNum Nothing
          move insertSlice empties
      spread' (Vector.tail elems) cells frequency (totalElems + frequency)

insertList :: (Ord k, Show k, Show v) => PMA k v -> [(k, v)] -> PMA k v
insertList = Prelude.foldl (\pma (k, v) -> Data.PMA.insert k v pma)

-- data New k a = New (forall s. ST s (PMA k a))

-- run :: New k v -> ST s (PMA  k a)
-- {-# INLINE run #-}
-- run (New p) = p

-- new :: New k a -> PMA k a
-- {-# INLINE_FUSED new #-}
-- new m = m `seq` runST (run m)

-- clone :: PMA k a -> New k a
-- {-# INLINE_FUSED clone #-}
-- clone pma = pma `seq` New (
--   do
--     newCells <- Vector.thaw $ cells pma
--                           )
