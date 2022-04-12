module VectorInteractive where

import qualified Data.Vector as V
import System.IO

main :: IO ()
main = do
  commands <- map read . lines <$> getContents
  runCommands commands (V.replicate 100 0)

runCommands :: [Command] -> V.Vector Amount -> IO ()
runCommands commands vec = do
  mapM_ print responses
  where
    responses = go commands vec
    go [] _vec = []
    go (command:moreCommands) vec = response : go moreCommands vec'
      where
        (vec', response) = runCommand command vec

runCommand :: Command -> V.Vector Amount -> (V.Vector Amount, String)
runCommand (Update i amount) vec = (V.update vec (V.fromList [(i, amount)]), "Ok.")
runCommand (RangeQuery i j) vec = (vec, show (V.sum (V.slice i j vec)))

type Amount = Int

data Command
  = Update Int Amount
  | RangeQuery Int Int
  deriving (Show, Read, Eq)
