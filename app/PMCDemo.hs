module PMCDemo where

import qualified Data.Vector as V
import qualified Data.PMAMap as PMC
import System.IO

main :: IO ()
main = do
  contents <- getContents
  let commands = V.fromList . lines $ contents
      -- (updates, queries) = V.partition (\cmd -> case cmd of Update _ _ -> True; RangeQuery _ _ -> False) commands
      (res, responses) = V.foldl (\(pmc, resps) textCommand ->
                       let command = read textCommand :: Command
                           (pmc', resp) = runCommand command pmc
                       in
                         case command of
                           Update _ _ -> (pmc', resps)
                           RangeQuery _ _ -> (pmc', resps <> V.singleton resp))

                                (PMC.empty, V.empty)
                                commands
  print res
  V.mapM_ print responses

type Amount = Int

runCommand :: Command -> PMC.Map Int Amount -> (PMC.Map Int Amount, String)
runCommand (Update i amount) pmc = (PMC.insertP i amount pmc, "Ok.")
runCommand (RangeQuery i j) pmc = (pmc, show ((+) <$> PMC.lookup i pmc <*> PMC.lookup j pmc))

data Command
  = Update Int Amount
  | RangeQuery Int Int
  deriving (Show, Read, Eq)
