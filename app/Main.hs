module Main (main) where

import Lib

main = do
    let (steps, ones, tape) = run fourRuleBeaverRuleA
    print tape
    putStrLn ("Steps: " <> show steps <> "; ones: " <> show ones)
