module VectorGrow where

import qualified Data.Vector as V

main :: IO ()
main = do
  values <- map read . lines <$> getContents :: IO [Int]
  let v = V.foldl (\acc x -> acc <> V.singleton x) V.empty $ V.fromList values
  print $ v V.! 9
