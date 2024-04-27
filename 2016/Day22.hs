module Day22 where

import Text.Regex.TDFA
import Data.List (sort, find)
import Data.Maybe (fromMaybe)

day22 :: IO ()
day22 = do
    s <- getContents
    let diskUsage = map getDiskUsage $ drop 2 $ lines s
    print $ countViablePairs diskUsage
    print $ getMinMoves diskUsage

data UsageData = UsageData {getX :: Int, getY :: Int, getSize :: Int, getUsed :: Int, getAvail :: Int}
dummyUsage :: UsageData
dummyUsage =  UsageData {getX = 0, getY = 0, getSize = 0, getUsed = 0, getAvail = 0}

getDiskUsage :: String -> UsageData
getDiskUsage s =
    case nums of
        [x, y, size, used, avail, _] -> UsageData {getX = x, getY = y, getSize = size, getUsed = used, getAvail = avail}
        _ -> dummyUsage
    where
        nums = map read $ getAllTextMatches (s =~ "[0-9]+")

countViablePairs :: [UsageData] -> Int
countViablePairs diskUsage = countSorted sortedUsed sortedAvail len len - samePairCount
    where
        len = length diskUsage
        sortedUsed = sort [getUsed usage | usage <- diskUsage, getUsed usage > 0]
        sortedAvail = sort [getAvail usage | usage <- diskUsage]
        samePairCount = sum [1 | usage <- diskUsage, getUsed usage > 0, getUsed usage <= getAvail usage]

countSorted :: [Int] -> [Int] -> Int -> Int -> Int
countSorted [] _ _ _ = 0
countSorted _ [] _ _ = 0
countSorted (x:xs) (y:ys) lx ly
    | y < x         = countSorted (x:xs) ys lx (ly-1)
    | otherwise     = countSorted xs (y:ys) (lx-1) ly + ly

getMinMoves :: [UsageData] -> Int
getMinMoves diskUsage =
    abs (getX emptyNode - (wallMinX - 1)) + getY emptyNode + (maxX - wallMinX + 1) + (maxX - 1) * 5
    where
        maxX = maximum [getX usage | usage <- diskUsage]
        minSize = minimum [getSize usage | usage <- diskUsage]
        -- assume certain input properties
        wall = [(getX usage, getY usage) | usage <- diskUsage, getUsed usage > minSize]
        wallMinX = minimum $ map fst wall
        emptyNode = fromMaybe dummyUsage $ find ((== 0) . getUsed) diskUsage
