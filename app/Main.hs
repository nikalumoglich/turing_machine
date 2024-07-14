module Main (main) where

import Lib

main = do
    let (steps, ones) = run fourRuleBeaverRuleA
    putStrLn ("Steps: " <> show steps <> "; ones: " <> show ones)
