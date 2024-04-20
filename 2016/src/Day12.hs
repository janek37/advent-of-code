module Day12 where

import System.IO
import Data.Char (isDigit)
import Data.IntMap.CharMap2
import Data.List (findIndex)


day12 = do
    s <- getContents
    let instructions = optimize $ Prelude.map (parseLine . words) (lines s)
    let registers = run instructions $ fromList [('a', 0), ('b', 0), ('c', 0), ('d', 0)]
    print (registers ! 'a')
    let registers = run instructions $ fromList [('a', 0), ('b', 0), ('c', 1), ('d', 0)]
    print (registers ! 'a')    


data Instruction = Set Int Char | Copy Char Char | Inc Char | Dec Char | Jnz Char Int | Jump Int | Add Char Char | Noop deriving Show

parseLine :: [String] -> Instruction
parseLine parts
    | head parts == "cpy"       =
        if isDigit $ head $ parts !! 1 then
            Set (read $ parts !! 1) (head $ parts !! 2)
        else
            Copy (head $ parts !! 1) (head $ parts !! 2)
    | head parts == "inc"       = Inc $ head $ parts !! 1
    | head parts == "dec"       = Dec $ head $ parts !! 1
    | head parts == "jnz"       =
        if isDigit $ head $ parts !! 1 then
            Jump (read $ parts !! 2)
        else
            Jnz (head $ parts !! 1) (read $ parts !! 2)

newtype State = State (Int, CharMap Int) deriving Show

nextState :: [Instruction] -> State -> State
nextState instructions (State (ip, registers)) =
    case instructions !! ip of
        Set n reg -> State (ip+1, insert reg n registers)
        Copy reg1 reg2 -> State (ip+1, insert reg2 (registers ! reg1) registers)
        Inc reg -> State (ip+1, insert reg (registers ! reg + 1) registers)
        Dec reg -> State (ip+1, insert reg (registers ! reg - 1) registers)
        Jnz reg n -> let value = registers ! reg in State (if value == 0 then ip + 1 else ip + n, registers)
        Jump n -> State (ip + n, registers)
        Add reg1 reg2 -> State (ip+1, insert reg1 0 $ insert reg2 (registers ! reg1 + registers ! reg2) registers)
        Noop -> State (ip+1, registers)

isValidState :: [Instruction] -> State -> Bool
isValidState instructions (State (ip, _)) = ip >= 0 && ip < length instructions

run :: [Instruction] -> CharMap Int -> CharMap Int
run instructions registers = finalRegisters
    where State (_, finalRegisters) = until (not . isValidState instructions) (nextState instructions) (State (0, registers))

optimize :: [Instruction] -> [Instruction]
optimize instructions =
    case maybeOptimizeAddition prefix of
        Just optimized -> optimized ++ optimize (drop 3 instructions)
        Nothing -> if Prelude.null instructions then [] else head instructions : optimize (tail instructions)
    where prefix = take 3 instructions

maybeOptimizeAddition instructions =
    case instructions of
        [Inc reg1, Dec reg2, Jnz reg3 n] -> if reg2 == reg3 && n == -2 then Just [Add reg2 reg1, Noop, Noop] else Nothing
        _ -> Nothing

enumerate =
    enumerateFrom 0
    where
        enumerateFrom i [] = []
        enumerateFrom i (h : t) = (i, h) : enumerateFrom (i+1) t
