module VectorPopulate where

import qualified Data.Vector as V
import System.IO


main :: IO ()
main = do
  withFile "number-input.txt" WriteMode writeFile
  where
    writeFile :: Handle -> IO ()
    writeFile handle = do
      let v1 = V.fromList [1..2*10^7]
          v2 = V.reverse v1
          v = V.zipWith (+) v1 v2
          l1 = [1..2*10^7]
          l2 = reverse l1
          -- l =  map (\(x,y) -> x+y) $ zip l1 l2
          l = V.toList v
      sequence $ map (hPutStrLn handle . show) l1
      -- hPutStrLn handle . show $ (foldl (+) 0 l) --
      -- hPutStrLn handle . show $ (foldl (+) 0 v)
      -- hPutStr handle . unlines $ map show l
      -- hPutStr handle . unlines . V.toList $ V.map show v
      return ()
      -- V.foldl (\acc x -> acc >> (hPutStrLn handle (show (x `mod` 100)))) (return ()) v
      -- !!! show both!!
