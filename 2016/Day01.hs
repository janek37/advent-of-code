module Day01 where

import Data.Foldable (find)

data Direction = North | South | West | East

day01 :: IO ()
day01 = do
    s <- getLine
    print (distance (parseInstructions s))
    print (revisitedDistance (parseInstructions s))

parseInstructions :: String -> [(Char, Int)]
parseInstructions s =
    let p ch = ch == ',' || ch == ' ' in
    case dropWhile p s of
    "" -> []
    s' -> (head w, read (tail w) :: Int): parseInstructions s''
        where
            (w, s'') = break p s'

distance :: [(Char, Int)] -> Int
distance instructions = abs x + abs y
    where (x, y) = destination instructions

destination :: [(Char, Int)] -> (Int, Int)
destination instructions = fst (foldl turnAndMove ((0, 0), North) instructions)

turnAndMove :: ((Int, Int), Direction) -> (Char, Int) -> ((Int, Int), Direction)
turnAndMove (loc, dir) (side, dist) =
    (move loc newDir dist, newDir)
    where newDir = turn dir side

turn :: Direction -> Char -> Direction
turn dir 'L' = turnLeft dir
turn dir 'R' = turnRight dir
turn dir _ = dir

turnLeft, turnRight :: Direction -> Direction
turnLeft dir = case dir of
    North -> West
    West -> South
    South -> East
    East -> North
turnRight dir = case dir of
    North -> East
    East -> South
    South -> West
    West -> North

move :: (Int, Int) -> Direction -> Int -> (Int, Int)
move (x, y) dir dist = case dir of
    North -> (x, y - dist)
    South -> (x, y + dist)
    West -> (x - dist, y)
    East -> (x + dist, y)

revisitedDistance :: [(Char, Int)] -> Int
revisitedDistance instructions =
    case maybeLocation of
        Just (x, y) -> abs x + abs y
        Nothing -> 0
    where maybeLocation =revisited (0, 0) North instructions [(0, 0)]

revisited :: (Int, Int) -> Direction -> [(Char, Int)] -> [(Int, Int)] -> Maybe (Int, Int)
revisited _ _ [] _ = Nothing
revisited pos dir ((side, dist):instructions) visited =
    let newSegment = segment pos (turn dir side) dist
        intersection = find (`elem` visited) newSegment
    in
    case intersection of
        Nothing -> let newDir = turn dir side in
            revisited (move pos newDir dist) newDir instructions (newSegment ++ visited)
        Just thePlace -> Just thePlace

segment :: (Int, Int) -> Direction -> Int -> [(Int, Int)]
segment pos dir n
    | n == 0 = []
    | otherwise = let nextPos = move pos dir 1 in
        nextPos:segment nextPos dir (n - 1)
