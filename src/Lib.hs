{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lib
    ( run
    , oneRuleBeaverRuleA
    , twoRuleBeaverRuleA
    , threeRuleBeaverRuleA
    , fourRuleBeaverRuleA
    , fiveRuleBeaverRuleA
    ) where

data Alphabet = Zero | One deriving (Show, Eq)

data Direction = Left' | Right' deriving (Show, Eq)

data Rule =
    Halt
    | Rule {
        zeroIntruction :: Instruction,
        oneInstruction :: Instruction
    } deriving (Show, Eq)

data Instruction = Instruction {
    printSymbol :: Alphabet,
    direction :: Direction,
    nextRule :: Rule
} deriving (Show, Eq)

oneRuleBeaverRuleAInstructionZero = Instruction { printSymbol = One, direction = Right', nextRule = Halt }
oneRuleBeaverRuleAInstructionOne = Instruction { printSymbol = One, direction = Right', nextRule = Halt }
oneRuleBeaverRuleA = Rule { zeroIntruction = oneRuleBeaverRuleAInstructionZero, oneInstruction = oneRuleBeaverRuleAInstructionOne }

------------------------

twoRuleBeaverRuleAInstructionZero = Instruction { printSymbol = One, direction = Right', nextRule = twoRuleBeaverRuleB }
twoRuleBeaverRuleAInstructionOne = Instruction { printSymbol = One, direction = Left', nextRule = twoRuleBeaverRuleB }
twoRuleBeaverRuleA = Rule { zeroIntruction = twoRuleBeaverRuleAInstructionZero, oneInstruction = twoRuleBeaverRuleAInstructionOne }

twoRuleBeaverRuleBInstructionZero = Instruction { printSymbol = One, direction = Left', nextRule = twoRuleBeaverRuleA }
twoRuleBeaverRuleBInstructionOne = Instruction { printSymbol = One, direction = Right', nextRule = Halt }
twoRuleBeaverRuleB = Rule { zeroIntruction = twoRuleBeaverRuleBInstructionZero, oneInstruction = twoRuleBeaverRuleBInstructionOne }

-----------------------

threeRuleBeaverRuleAInstructionZero = Instruction { printSymbol = One, direction = Right', nextRule = threeRuleBeaverRuleB }
threeRuleBeaverRuleAInstructionOne = Instruction { printSymbol = One, direction = Right', nextRule = Halt }
threeRuleBeaverRuleA = Rule { zeroIntruction = threeRuleBeaverRuleAInstructionZero, oneInstruction = threeRuleBeaverRuleAInstructionOne }

threeRuleBeaverRuleBInstructionZero = Instruction { printSymbol = Zero, direction = Right', nextRule = threeRuleBeaverRuleC }
threeRuleBeaverRuleBInstructionOne = Instruction { printSymbol = One, direction = Right', nextRule = threeRuleBeaverRuleB }
threeRuleBeaverRuleB = Rule { zeroIntruction = threeRuleBeaverRuleBInstructionZero, oneInstruction = threeRuleBeaverRuleBInstructionOne }

threeRuleBeaverRuleCInstructionZero = Instruction { printSymbol = One, direction = Left', nextRule = threeRuleBeaverRuleC }
threeRuleBeaverRuleCInstructionOne = Instruction { printSymbol = One, direction = Left', nextRule = threeRuleBeaverRuleA }
threeRuleBeaverRuleC = Rule { zeroIntruction = threeRuleBeaverRuleCInstructionZero, oneInstruction = threeRuleBeaverRuleCInstructionOne }

-------

fourRuleBeaverRuleAInstructionZero = Instruction { printSymbol = One, direction = Right', nextRule = fourRuleBeaverRuleB }
fourRuleBeaverRuleAInstructionOne = Instruction { printSymbol = One, direction = Left', nextRule = fourRuleBeaverRuleB }
fourRuleBeaverRuleA = Rule { zeroIntruction = fourRuleBeaverRuleAInstructionZero, oneInstruction = fourRuleBeaverRuleAInstructionOne }

fourRuleBeaverRuleBInstructionZero = Instruction { printSymbol = One, direction = Left', nextRule = fourRuleBeaverRuleA }
fourRuleBeaverRuleBInstructionOne = Instruction { printSymbol = Zero, direction = Left', nextRule = fourRuleBeaverRuleC }
fourRuleBeaverRuleB = Rule { zeroIntruction = fourRuleBeaverRuleBInstructionZero, oneInstruction = fourRuleBeaverRuleBInstructionOne }

fourRuleBeaverRuleCInstructionZero = Instruction { printSymbol = One, direction = Right', nextRule = Halt }
fourRuleBeaverRuleCInstructionOne = Instruction { printSymbol = One, direction = Left', nextRule = fourRuleBeaverRuleD }
fourRuleBeaverRuleC = Rule { zeroIntruction = fourRuleBeaverRuleCInstructionZero, oneInstruction = fourRuleBeaverRuleCInstructionOne }

fourRuleBeaverRuleDInstructionZero = Instruction { printSymbol = One, direction = Right', nextRule = fourRuleBeaverRuleD }
fourRuleBeaverRuleDInstructionOne = Instruction { printSymbol = Zero, direction = Right', nextRule = fourRuleBeaverRuleA }
fourRuleBeaverRuleD = Rule { zeroIntruction = fourRuleBeaverRuleDInstructionZero, oneInstruction = fourRuleBeaverRuleDInstructionOne }

------

fiveRuleBeaverRuleAInstructionZero = Instruction { printSymbol = One, direction = Right', nextRule = fiveRuleBeaverRuleB }
fiveRuleBeaverRuleAInstructionOne = Instruction { printSymbol = One, direction = Left', nextRule = fiveRuleBeaverRuleC }
fiveRuleBeaverRuleA = Rule { zeroIntruction = fiveRuleBeaverRuleAInstructionZero, oneInstruction = fiveRuleBeaverRuleAInstructionOne }

fiveRuleBeaverRuleBInstructionZero = Instruction { printSymbol = One, direction = Right', nextRule = fiveRuleBeaverRuleC }
fiveRuleBeaverRuleBInstructionOne = Instruction { printSymbol = One, direction = Right', nextRule = fiveRuleBeaverRuleB }
fiveRuleBeaverRuleB = Rule { zeroIntruction = fiveRuleBeaverRuleBInstructionZero, oneInstruction = fiveRuleBeaverRuleBInstructionOne }

fiveRuleBeaverRuleCInstructionZero = Instruction { printSymbol = One, direction = Right', nextRule = fiveRuleBeaverRuleD }
fiveRuleBeaverRuleCInstructionOne = Instruction { printSymbol = Zero, direction = Left', nextRule = fiveRuleBeaverRuleE }
fiveRuleBeaverRuleC = Rule { zeroIntruction = fiveRuleBeaverRuleCInstructionZero, oneInstruction = fiveRuleBeaverRuleCInstructionOne }

fiveRuleBeaverRuleDInstructionZero = Instruction { printSymbol = One, direction = Left', nextRule = fiveRuleBeaverRuleA }
fiveRuleBeaverRuleDInstructionOne = Instruction { printSymbol = One, direction = Left', nextRule = fiveRuleBeaverRuleD }
fiveRuleBeaverRuleD = Rule { zeroIntruction = fiveRuleBeaverRuleDInstructionZero, oneInstruction = fiveRuleBeaverRuleDInstructionOne }

fiveRuleBeaverRuleEInstructionZero = Instruction { printSymbol = One, direction = Right', nextRule = Halt }
fiveRuleBeaverRuleEInstructionOne = Instruction { printSymbol = Zero, direction = Left', nextRule = fiveRuleBeaverRuleA }
fiveRuleBeaverRuleE = Rule { zeroIntruction = fiveRuleBeaverRuleEInstructionZero, oneInstruction = fiveRuleBeaverRuleEInstructionOne }

run :: (Num t1, Num t2) => Rule -> (t2, t1, [Alphabet])
run rule = run' rule 0 0 [Zero] 0
    where
        run' rule steps ones tape tapePosition =
            case rule of
                Halt -> (steps, ones, tape)
                Rule zeroIntruction oneInstruction ->
                    case currentValue of
                        Zero -> runStep zeroIntruction steps ones tape tapePosition
                        One -> runStep oneInstruction steps ones tape tapePosition
                        where
                            currentValue = tape !! tapePosition
                            runStep instruction steps ones tape tapePosition =  run' nextRule' newSteps newOnes newTape newTapePosition
                                where
                                    nextRule' = nextRule instruction
                                    newSteps = steps + 1
                                    newOnes = case currentValue of
                                        Zero -> if printSymbol instruction == One then ones + 1 else ones
                                        One -> if printSymbol instruction == Zero then ones - 1 else ones
                                    newTape = case direction instruction of
                                        Left' ->
                                            if tapePosition == 0 then 
                                                Zero : printSymbol instruction : tail tape 
                                            else
                                                tapeBeforeCurrentPosition <> [printSymbol instruction] <> tapeAfterCurrentPosition
                                        Right' -> 
                                            if tapePosition == length tape - 1 then
                                                tapeBeforeCurrentPosition <> [printSymbol instruction] <> [Zero]
                                            else
                                                tapeBeforeCurrentPosition <> [printSymbol instruction] <> tapeAfterCurrentPosition
                                        where
                                            tapeBeforeCurrentPosition = take tapePosition tape
                                            tapeAfterCurrentPosition = drop (tapePosition + 1) tape
                                    newTapePosition = case direction instruction of
                                        Left' -> if tapePosition == 0 then 0 else tapePosition - 1
                                        Right' -> if tapePosition == length tape - 1 then length tape else tapePosition + 1


