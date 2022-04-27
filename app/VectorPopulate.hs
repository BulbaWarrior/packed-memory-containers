module VectorPopulate where

import qualified Data.Vector as V
import System.IO

main :: IO ()
main = do
  withFile "number-input.txt" WriteMode writeFile
  where
    writeFile :: Handle -> IO ()
    writeFile handle = do
      let v = V.fromList [1..2*10^7]
      -- V.sequence $ V.map (hPutStrLn handle . show) v
      V.foldl (\acc x -> acc >> (hPutStrLn handle (show (x `mod` 100)))) (return ()) v
      return ()
