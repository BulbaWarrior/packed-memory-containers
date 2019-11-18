{-# OPTIONS_GHC -fdefer-type-errors #-}
module Data.PackedMemoryArray where

import qualified Data.List   as List
import           Prelude
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

data PMA a = PMA
  { capacity :: Int                       -- total capacity of PMA
  , segmentCapacity :: Int                -- the size of single segment
  , height :: Int                         -- the height of the binary tree for elements
  , elements :: Vector (Maybe a)          -- the elements contained
  , cardinality :: Int                    -- the number of elements contained
  , segmentsCnt :: Int                    -- the number of segments
  , segmentsCardinalities :: Vector Int   -- the number of elements contained in current segment
  } deriving (Show)

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

log2 :: Int -> Double
log2 value = logBase 2 (fromIntegral value)

-- Round up to closest power of 2
hyperceil :: Int -> Int
hyperceil value = 2 ^ (ceiling (log2 value))

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

setElement :: PMA a -> Int -> Maybe a -> PMA a
setElement pma pos v = pma
  { elements = Vector.update (elements pma) (Vector.fromList [(pos, v)]) }

-- init empty PMA (capacity == minCapacity)
emptyPMA :: PMA a
emptyPMA = initPMA minCapacity

clearPMA :: PMA a -> PMA a
clearPMA _ = emptyPMA

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
  where
    windowEnd = windowStart + windowLength
    tmp = getValidValues (subVector (elements pma) startPos len)
      where
        startPos = (segmentCapacity pma) * windowStart
        len = (segmentCapacity pma) * windowLength

        subVector :: Vector (Maybe a) -> Int -> Int -> Vector (Maybe a)
        subVector vec start len = Vector.take len (Vector.drop start vec)

        getValidValues :: Vector (Maybe a) -> Vector (Maybe a)
        getValidValues vec
          | null vec  = Vector.empty
          | otherwise = takeIfValid (vec Vector.! 0) <> (Vector.drop 0 vec)
          where
            takeIfValid :: Maybe a -> Vector (Maybe a)
            takeIfValid (Just val)  = Vector.singleton (Just val)
            takeIfValid _           = Vector.empty

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
          (Vector.take startPos origVector) <> newSubVector <> (Vector.drop (startPos + (Vector.length newSubVector) - 1) origVector)

    newElements = replaceSubVector (windowStart * (segmentCapacity pma)) newSubElements (elements pma)
      where
        replaceSubVector :: Int -> Vector (Maybe a) -> Vector (Maybe a) -> Vector (Maybe a)
        replaceSubVector startPos newSubVector origVector =
          (Vector.take startPos origVector) <> newSubVector <> (Vector.drop (startPos + (Vector.length newSubVector) - 1) origVector)


insert :: Ord a => a -> PMA a -> PMA a
insert x _ = undefined

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

getElem :: Vector a -> Int -> a
getElem vec i = vec Vector.! i

run :: IO ()
run = do
  let vec = Vector.fromList [5, 6, 7]
  print "Hello, Azat!"
  -- addElement Vector.empty
