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



main :: IO ()
main = do
  withFile "input.txt" ReadMode handleFile
  where
    handleFile :: Handle -> IO ()
    handleFile handle = do
      content <- hGetContents handle
      let vector = V.fromList $ lines content
      print $ vector V.! 123
