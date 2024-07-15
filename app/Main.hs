module Main (main) where

import Lib

main = do
    let (steps, ones, tape) = run fiveRuleBeaverRuleA
    print tape
    putStrLn ("Steps: " <> show steps <> "; ones: " <> show ones)
