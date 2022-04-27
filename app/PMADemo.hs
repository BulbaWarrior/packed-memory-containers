module PMCDemo where

import qualified Data.Vector as V
import qualified Data.PMAMap as PMC
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

type Amount = Int

runCommand :: Command -> PMC.Map Int Amount -> (PMC.Map Int Amount, String)
