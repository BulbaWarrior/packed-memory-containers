{-# OPTIONS_GHC -fdefer-type-errors -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- TBD.
module Data.PackedMemoryArray where

import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Prelude

-- import           Debug.Trace

-- * PMA type

-- | Packed-memory array.
data PMA a = PMA
  { capacity              :: Int              -- ^ Total capacity of PMA.
  , segmentCapacity       :: Int              -- ^ Size of each segment.
  , height                :: Int              -- ^ Height of the binary tree for elements.
  , elements              :: Vector (Maybe a) -- ^ Vector of all cells (elements or gaps).
  , cardinality           :: Int              -- ^ Number of elements contained.
  , segmentsCnt           :: Int              -- ^ Number of segments
  , segmentsCardinalities :: Vector Int       -- ^ Number of elements contained in each segment.
  } deriving (Show, Eq, Foldable)

-- * Constants

minCapacity :: Int
minCapacity = 8

t_h :: Double
t_h = 0.75

-- t_0 :: Double
-- t_0 = 1.0

p_h :: Double
p_h = 0.5

-- p_0 :: Double
-- p_0 = 0.1

-- * Construction

-- init PMA with given capacity
initPMA :: Int -> PMA a
initPMA c = PMA
  { capacity = hyperceil c
  , segmentCapacity = hyperceil c
  , height = 1
  , elements = Vector.replicate (hyperceil c) Nothing
  , cardinality = 0
  , segmentsCnt = 1
  , segmentsCardinalities = Vector.singleton 0
  }


-- | Empty packed-memory array.
--
-- prop> capacity emptyPMA == minCapacity
emptyPMA :: PMA a
emptyPMA = initPMA minCapacity

-- * Insertion

-- | Worst case: \(O(n)\).
--
-- Insert an element into a packed-memory array.
insert :: forall a. (Ord a, Show a) => PMA a -> a -> PMA a
insert pma val = if (((segmentsCardinalities newPMA) Vector.! segmentId) == (segmentCapacity pma)) then (rebalance newPMA segmentId) else newPMA
  where
    segmentId = findSegment pma val
    (elements', posToInsert) = findPos (elements pma) ((segmentsCardinalities pma) Vector.! segmentId)
      where
        findPos :: Vector (Maybe a) -> Int -> (Vector (Maybe a), Int)
        findPos vec pos = if (pos > 0) && ((Just val) < (vec Vector.! (pos - 1)))
                        then findPos (Vector.update vec (Vector.fromList [(pos - 1, Nothing), (pos, vec Vector.! (pos - 1))])) (pos - 1)
                        else (vec, pos)

    newElements = Vector.update elements' (Vector.singleton (posToInsert, Just val))
    newPMA = PMA
            { capacity = capacity pma
            , segmentCapacity = segmentCapacity pma
            , height = height pma
            , elements = newElements
            , cardinality = (cardinality pma) + 1
            , segmentsCnt = segmentsCnt pma
            , segmentsCardinalities = Vector.update (segmentsCardinalities pma) (Vector.singleton (segmentId, ((segmentsCardinalities pma) Vector.! segmentId) + 1))
            }

-- * Delete/Update

setElement :: PMA a -> Int -> Maybe a -> PMA a
setElement pma pos v = pma
  { elements = Vector.update (elements pma) (Vector.fromList [(pos, v)]) }

clearPMA :: PMA a -> PMA a
clearPMA _ = emptyPMA

-- * Query

isEmpty :: PMA a -> Bool
isEmpty pma = (cardinality pma) == 0

-- | TODO: switch to binsearch.
elemIndex :: Eq a => a -> PMA a -> Maybe Int
elemIndex pma val = Just val `Vector.elemIndex` elements pma

-- * Helpers

log2 :: Int -> Double
log2 value = logBase 2 (fromIntegral value)

-- Round up to closest power of 2
hyperceil :: Int -> Int
hyperceil value = 2 ^ (ceiling (log2 value))

-- Get the lower and upper segments for the given segment_id at the given level
-- level >= 1
-- (l, r) both inclusive
windowBounds :: PMA a -> Int -> Int -> (Int, Int)
windowBounds pma pos level = (l, r)
  where
    windowCapacity = (segmentCapacity pma) * (2 ^ (level - 1))
    l = (pos `div` windowCapacity) * windowCapacity
    r = l + windowCapacity - 1

-- Get the lower (p_l) and upper (t_l) threshold for the windows at the given level
-- level >= 1
windowThresholds :: PMA a -> Int -> (Double, Double)
windowThresholds pma level = (p_l, t_l)
  where
    h = height pma
    diff = fromIntegral (h - level) / fromIntegral h
    p_l = p_h - 0.25 * diff
    t_l = t_h + 0.25 * diff

resize :: PMA a -> PMA a
resize pma = PMA
  { capacity = newCapacity
  , segmentCapacity = newSegmentCapacity
  , height = floor (log2 (newCapacity `div` newSegmentCapacity)) + 1
  , elements = newElements
  , cardinality = cardinality pma
  , segmentsCnt = newSegmentsCnt
  , segmentsCardinalities = newSegmentsCardinalities
  }
    where
      newCapacity = (capacity pma) * 2
      newSegmentCapacity = hyperceil (floor (log2 newCapacity))
      newSegmentsCnt = newCapacity `div` newSegmentCapacity
      elementsPerSegment = (cardinality pma) `div` newSegmentsCnt
      oddSegmentsCnt = (cardinality pma) `mod` newSegmentsCnt

      newSegmentsCardinalities = getSegmentsCardinalities 0
        where
          getSegmentsCardinalities :: Int -> Vector Int
          getSegmentsCardinalities i
            | i == newSegmentsCnt = Vector.empty
            | i < oddSegmentsCnt  = (Vector.singleton (elementsPerSegment + 1)) <> (getSegmentsCardinalities (i + 1))
            | otherwise           = (Vector.singleton elementsPerSegment) <> (getSegmentsCardinalities (i + 1))

      newElements = getElements (elements pma) newSegmentsCardinalities
        where
          getElements :: Vector (Maybe a) -> Vector Int -> Vector (Maybe a)
          getElements elems sizes
            | null sizes  = Vector.empty
            | otherwise   = curSegment <> (getElements (Vector.drop curSize elems) sizesTail)
            where
              curSize = sizes Vector.! 0
              sizesTail = Vector.drop 1 sizes
              curSegment = (Vector.take curSize elems) <> (Vector.replicate (newSegmentCapacity - curSize) Nothing)


-- Equally spread the elements in the interval [segmentCapacity * windowStart, segmentCapacity * (windowStart + windowLength) )
spread :: PMA a -> Int -> Int -> Int -> PMA a
spread pma elementsNum windowStart windowLength = pma
  { segmentsCardinalities = newSegmentsCardinalities
  , elements = newElements
  }
  where
    windowEnd = windowStart + windowLength
    tmp = getValidValues (subVector (elements pma) startPos len)
      where
        startPos = (segmentCapacity pma) * windowStart
        len = (segmentCapacity pma) * windowLength

        subVector :: Vector (Maybe a) -> Int -> Int -> Vector (Maybe a)
        subVector vec start n = Vector.take n (Vector.drop start vec)

        getValidValues :: Vector (Maybe a) -> Vector (Maybe a)
        getValidValues vec
          | null vec  = Vector.empty
          | otherwise = takeIfValid (vec Vector.! 0) <> (Vector.drop 0 vec)
          where
            takeIfValid :: Maybe a -> Vector (Maybe a)
            takeIfValid (Just val) = Vector.singleton (Just val)
            takeIfValid _          = Vector.empty

    elementsPerSegment = elementsNum `div` windowLength
    oddSegmentsCnt = elementsNum `mod` windowLength

    newSubSegmentsCardinalities = getSegmentsCardinalities 0
      where
        getSegmentsCardinalities :: Int -> Vector Int
        getSegmentsCardinalities i
          | i == windowLength   = Vector.empty
          | i < oddSegmentsCnt  = (Vector.singleton (elementsPerSegment + 1)) <> (getSegmentsCardinalities (i + 1))
          | otherwise           = (Vector.singleton elementsPerSegment) <> (getSegmentsCardinalities (i + 1))

    newSubElements = getElements (elements pma) newSubSegmentsCardinalities
      where
        getElements :: Vector (Maybe a) -> Vector Int -> Vector (Maybe a)
        getElements elems sizes
          | null sizes  = Vector.empty
          | otherwise   = curSegment <> (getElements (Vector.drop curSize elems) sizesTail)
          where
            curSize = sizes Vector.! 0
            sizesTail = Vector.drop 1 sizes
            curSegment = (Vector.take curSize elems) <> (Vector.replicate ((segmentCapacity pma) - curSize) Nothing)

    newSegmentsCardinalities = replaceSubVector windowStart newSubSegmentsCardinalities (segmentsCardinalities pma)
      where
        replaceSubVector :: Int -> Vector Int -> Vector Int -> Vector Int
        replaceSubVector startPos newSubVector origVector =
          (Vector.take startPos origVector) <> newSubVector <> (Vector.drop (startPos + (Vector.length newSubVector)) origVector)

    newElements = replaceSubVector (windowStart * (segmentCapacity pma)) newSubElements (elements pma)
      where
        replaceSubVector :: Int -> Vector (Maybe a) -> Vector (Maybe a) -> Vector (Maybe a)
        replaceSubVector startPos newSubVector origVector =
          (Vector.take startPos origVector) <> newSubVector <> (Vector.drop (startPos + (Vector.length newSubVector)) origVector)


rebalance :: PMA a -> Int -> PMA a
rebalance pma segmentId = rebalancedPMA
  where
    (newDensity, t_l, elementsCnt, windowStart, windowLength) =
      if ((height pma) > 1)
        then (tryHigher 2 2 (segmentId `div` 2))
        else (1.0, t_h, ((segmentsCardinalities pma) Vector.! segmentId), segmentId, 1)
      where
        tryHigher :: Int -> Int -> Int -> (Double, Double, Int, Int, Int)
        tryHigher l windowLength windowId = if ((density >= t_l) && l < (height pma)) then tryHigher (l + 1) (windowLength * 2) (windowId `div` 2) else (density, t_l, elementsCnt, windowStart, windowLength)
          where
            windowStart = windowId * windowLength --inclusive
            windowEnd = windowStart + windowLength --exclusive
            (p_l, t_l) = windowThresholds pma l
            prefixSums = Vector.postscanl (+) 0 (segmentsCardinalities pma)
            elementsCnt = if (windowStart == 0)
                        then (prefixSums Vector.! (windowEnd - 1))
                        else ((prefixSums Vector.! (windowEnd - 1)) - (prefixSums Vector.! windowStart))
            density = (fromIntegral elementsCnt) / (fromIntegral (windowLength * (segmentCapacity pma)))

    rebalancedPMA = if (newDensity >= t_l) then (resize pma) else (spread pma elementsCnt windowStart windowLength)



findSegment :: forall a. Ord a => PMA a -> a -> Int
findSegment pma val = if (isEmpty pma) then 0 else (find pma 0 ((segmentsCnt pma) - 1))
  where
    find :: PMA a -> Int -> Int -> Int
    find pma lb ub = if (lb < ub) then (find pma newLB newUB) else lb
      where
        mid = (lb + ub) `div` 2
        (newLB, newUB) =  if ((Just val) < ((elements pma) Vector.! (mid * (segmentCapacity pma))))
                        then (lb, mid - 1)
                        else  if ((Just val) <= ((elements pma) Vector.! ((mid * (segmentCapacity pma)) + ((segmentsCardinalities pma) Vector.! mid) - 1)))
                            then (mid, mid)
                            else (mid + 1, ub)

-- * Tests

samplePMA_1 :: PMA Int
samplePMA_1 = PMA
  { capacity = 8
  , segmentCapacity = 4
  , height = 2
  , elements = Vector.fromList
      [ Just 1, Just 2, Just 3, Just 4
      , Nothing, Nothing, Nothing, Nothing
      ]
  , cardinality = 4
  , segmentsCnt = 2
  , segmentsCardinalities = Vector.fromList [4, 0]
  }

samplePMA_2 :: PMA Int
samplePMA_2 = PMA
  { capacity = 8,
    segmentCapacity = 4,
    height = 2,
    elements = Vector.fromList
      [ Just 1, Just 2, Nothing, Nothing
      , Just 3, Just 4, Nothing, Nothing
      ]
    , cardinality = 4
    , segmentsCnt = 2
    , segmentsCardinalities = Vector.fromList [2,2]
  }

samplePMA_3 :: PMA Int
samplePMA_3 = PMA
  { capacity = 8,
    segmentCapacity = 8,
    height = 1,
    elements = Vector.fromList
      [ Just 1, Just 2, Just 3, Just 4
      , Just 5, Just 6, Just 7, Nothing
      ]
    , cardinality = 7
    , segmentsCnt = 1
    , segmentsCardinalities = Vector.fromList [7]
  }

samplePMA_4 :: PMA Int
samplePMA_4 = PMA
  { capacity = 16
  , segmentCapacity = 4
  , height = 3
  , elements = Vector.fromList
      [ Just 1,Just 2,Nothing,Nothing
      , Just 4,Just 5,Nothing,Nothing
      , Just 6,Just 7,Nothing,Nothing
      , Just 8,Just 9,Nothing,Nothing
      ]
  , cardinality = 8
  , segmentsCnt = 4
  , segmentsCardinalities = Vector.fromList [2,2,2,2]
}

addElement :: Vector String -> IO()
addElement list = do
  print list
  command <- getLine
  if command == "+" then
    do
      el <- getLine
      addElement (Vector.snoc list el)
    else
        undefined

run :: IO ()
run = do
  print "Hello, Azat!"
  -- addElement Vector.empty
