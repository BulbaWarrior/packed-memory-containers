{-# OPTIONS_GHC -ddump-to-file -ddump-rule-rewrites -fenable-rewrite-rules -ddump-simpl #-}

module PMCFusion where
import Data.PMAMap as PMC

main :: IO ()
main = do
  let pmc1 = insert 1 2 empty
      pmc2 = insert 3 4 pmc1
      pmc3 = insert 5 6 pmc2
  print $ PMC.lookup 5 pmc3
