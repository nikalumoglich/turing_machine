import Lib

main :: IO ()
main = do
    let (steps, ones) = run fourRuleBeaverRuleA
    putStrLn ("Steps: " <> show steps <> "; ones: " <> show ones)
