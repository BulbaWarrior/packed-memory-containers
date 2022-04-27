module VectorInteractive where

import qualified Data.Vector as V
import System.IO

main :: IO ()
main = do
  contents <- getContents
  let commands = V.fromList . lines $ contents
      -- (updates, queries) = V.partition (\cmd -> case cmd of Update _ _ -> True; RangeQuery _ _ -> False) commands
      (res, responses) = V.foldl (\(vec, resps) textCommand ->
                       let command = read textCommand :: Command
                           (vec', resp) = runCommand command vec
                       in
                         case command of
                           Update _ _ -> (vec', resps)
                           RangeQuery _ _ -> (vec', resps <> V.singleton resp))
                    (V.replicate 100 0, V.empty)
                    commands
  print res
  V.mapM_ print responses



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
