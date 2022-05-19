{-# OPTIONS_GHC -ddump-to-file -ddump-rule-rewrites -fenable-rewrite-rules -ddump-simpl #-}

module VectorRead where

import qualified Data.Vector as V
import System.IO

fromList :: [a] -> V.Vector a
fromList [] = V.empty
fromList (x:xs) = V.singleton x <> fromList xs

fromList2 :: V.Vector a -> [a] -> V.Vector a
fromList2 vec [] = vec
fromList2 vec (x:xs) = fromList2 (vec <> V.singleton x) xs

fromList3 :: [a] -> V.Vector a
fromList3 = foldr (\x xs -> V.singleton x <> xs) V.empty

accum :: Num a => V.Vector a -> V.Vector a
accum vec = go n vec
  where
    n = length vec
    go i v =
      let x = V.sum (V.slice 0 i v)
       in v <> V.fromList [x]

at :: [a] -> Int -> Maybe a
at [] _ = Nothing
at (x:xs) 0 = Just x
at (x:xs) n = at xs (n-1)


main :: IO ()
main = do
  withFile "number-input.txt" ReadMode handleFile
  where
    handleFile :: Handle -> IO ()
    handleFile handle = do
      content <- hGetContents handle
      let v1 = V.fromList $ map read . lines $ content
          v2 = V.fromList [1..2*10^7]
          v = V.zipWith (+) v1 v2
          l1 = map read . lines $ content
          l2 = [1..2*10^7]
          l = zipWith (+) l1 l2
      print $ V.foldl (+) 0 v
      -- print $ foldl (+) 0 l
