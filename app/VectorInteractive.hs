module VectorInteractive where

import qualified Data.Vector as V
import System.IO

main :: IO ()
main = do
  contents <- getContents
  let commands = V.fromList $ map read . lines $ contents
  runCommands commands (V.replicate 100 0)

runCommands :: V.Vector Command -> V.Vector Amount -> IO ()
runCommands commands vec = do
  mapM_ print $ V.fromList responses
  where
    responses = go commands vec
    go cmds _vec
      | V.null cmds = []
      | otherwise =
        let command = V.head cmds
            moreCommands = V.tail cmds
            (vec', response) = runCommand command vec
        in response : go moreCommands vec'

runCommand :: Command -> V.Vector Amount -> (V.Vector Amount, String)
runCommand (Update i amount) vec = (V.update vec (V.fromList [(i, amount)]), "Ok.")
runCommand (RangeQuery i j) vec = (vec, show (V.sum (V.slice i j vec)))

type Amount = Int

data Command
  = Update Int Amount
  | RangeQuery Int Int
  deriving (Show, Read, Eq)
