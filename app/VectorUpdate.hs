-- TODO dump simpl

{-# OPTIONS_GHC -ddump-to-file -ddump-rule-rewrites -fenable-rewrite-rules -ddump-simpl #-}
module VectorUpdate where

import qualified Data.Vector as V

main :: IO ()
main = fullFusion

fullFusion = do
  let v = V.fromList [1..10^5]
      newV = v V.// [(0, x) | x <- [1..10^5]]
  print $ newV V.! 9

partialFusion = do
  values <- map read . lines <$> getContents
  let v = V.fromList [1..10^5]
      newV = v V.// [(0, x) | x <- values]
  print $ newV V.! 9

noFusion = do
  let v = V.fromList [1..10^5]
      newV = V.foldl (\acc x -> V.update acc $ V.singleton (0, x)) v $ V.fromList [1..10^5]
  print $ newV V.! 9
