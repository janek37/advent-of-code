module Day11 where

import Text.Regex.TDFA
import Data.List (isSuffixOf, nub, findIndex, sort)
import Data.Function (fix)
import Data.Maybe ( fromMaybe )

day11 :: IO ()
day11 = do
    s <- getContents
    let state = stateFromFloors $ map parseLine (lines s)
    let moveCount = minMoves state
    mapM_ print moveCount

    let State (_, originalLocations) = state
    let newState = State (0, (0, 0):(0, 0):originalLocations)
    let newMoveCount = minMoves newState
    mapM_ print newMoveCount

data Equipment = Generator String | Microchip String deriving (Eq, Show)

parseLine :: String -> [Equipment]
parseLine s = map strToEquipment matches
    where
        matches = getAllTextMatches (s =~ "[a-z]+(-compatible microchip| generator)") :: [String]

element :: String -> String
element s = s =~ "[a-z]+"

equipmentType :: String -> (String -> Equipment)
equipmentType s = if "generator" `isSuffixOf` s then Generator else Microchip

strToEquipment :: String -> Equipment
strToEquipment s = equipmentType s (element s)

equipmentElement :: Equipment -> String
equipmentElement e =
    case e of
        Generator s -> s
        Microchip s -> s

newtype State = State (Int, [(Int, Int)]) deriving (Eq, Show)

stateFromFloors :: [[Equipment]] -> State
stateFromFloors floors =
    State (0, sort (map pair elements))
    where
        elements = nub $ map equipmentElement $ concat floors
        findFloor equipment = fromMaybe 0 (findIndex (equipment `elem`) floors)
        pair element' = (findFloor $ Generator element', findFloor $ Microchip element')

validMoves :: State -> [State]
validMoves state =
    concatMap (filter isValidState . validMovesTo state) adjacent
    where
        State (elevator, _) = state
        adjacent = adjacentFloors elevator

validMovesTo :: State -> Int -> [State]
validMovesTo (State (elevator, locations)) toFloor =
    nub $ map (\l -> State (toFloor, sort l)) allMoves
    where
        allSingle = singleMoves elevator toFloor locations
        allMoves = allSingle ++ if elevator < toFloor then doubleMoves elevator toFloor locations else []

singleMoves :: Int -> Int -> [(Int, Int)] -> [[(Int, Int)]]
singleMoves fromFloor toFloor =
    modifiedLists (\(x, y) -> [(toFloor, y) | x == fromFloor] ++ [(x, toFloor) | y == fromFloor])

modifiedLists :: (t -> [t]) -> [t] -> [[t]]
modifiedLists modify xs =
    case xs of
        [] -> []
        x : rest -> map (: rest) (modify x) ++ map (x :) (modifiedLists modify rest)

doubleMoves :: Int -> Int -> [(Int, Int)] -> [[(Int, Int)]]
doubleMoves fromFloor toFloor locations = concatMap (singleMoves fromFloor toFloor) (singleMoves fromFloor toFloor locations)


adjacentFloors :: (Eq a1, Num a1, Num a2) => a1 -> [a2]
adjacentFloors n
    | n == 0    = [1]
    | n == 1    = [0, 2]
    | n == 2    = [1, 3]
    | n == 3    = [2]
    | otherwise = []

isValidState :: State -> Bool
isValidState (State (elevator, locations)) =
    any (\(a, b) -> a == elevator || b == elevator) locations
    && all noGeneratorsOnFloor lonelyChipFloorIndexes
    where
        lonelyChipFloorIndexes = nub $ map snd $ filter (uncurry (/=)) locations
        noGeneratorsOnFloor n = not $ any ((== n) . fst) locations

evolve :: State -> [[State]] -> [[State]]
evolve state evolvedStates = [state] : map (nub . concatMap validMoves) evolvedStates

minMoves :: State -> Maybe Int
minMoves state = findIndex (any ((== 3) . minFloor)) evolvedStates
    where
        evolvedStates = fix (evolve state)

minFloor :: State -> Int
minFloor (State (_, locations)) = minimum (map (uncurry min) locations)
