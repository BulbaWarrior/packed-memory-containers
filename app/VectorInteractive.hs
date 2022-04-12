module VectorInteractive where

import qualified Data.Vector as V
import System.IO

main :: IO ()
main = do
  contents <- getContents
  let commands = V.map read . V.fromList . lines $ contents :: V.Vector Command
      (updates, queries) = V.partition (\cmd -> case cmd of Update _ _ -> True; RangeQuery _ _ -> False) commands
      res = V.foldl (\vec update -> let (vec', _) = runCommand update vec in vec') (V.replicate 100 0) updates
      responses = V.map (runQuery res) queries
  mapM_ print responses
  where runQuery = flip runCommand



runCommands :: V.Vector String -> V.Vector Amount -> IO ()
runCommands commands vec = do
  _ <- sequence $ map (\resp -> if resp == "Ok." then return () else print resp) responses
  return ()
  where
    responses = go commands vec
    go cmds _vec
      | V.null cmds = []
      | otherwise =
        let command = read . V.head $ cmds :: Command
            moreCommands = V.tail cmds
            (vec', response) = runCommand command vec
        in response : go moreCommands vec'


runCommand :: Command -> V.Vector Amount -> (V.Vector Amount, String)
runCommand (Update i amount) vec = (vec V.// [(i, amount)], "Ok.")
runCommand (RangeQuery i j) vec = (vec, show (V.sum (V.slice i j vec)))

type Amount = Int

data Command
  = Update Int Amount
  | RangeQuery Int Int
  deriving (Show, Read, Eq)
