module Day20 where
import Data.List (sort)

day20 :: IO ()
day20 = do
    s <- getContents
    let ranges = sort $ map getRange $ lines s
    print $ firstOmittedFrom 0 ranges
    print $ 4294967296 - countIncluded ranges


firstOmittedFrom :: Int -> [(Int, Int)] -> Int
firstOmittedFrom n [] = n
firstOmittedFrom n ((a, b):rest)
    | n < a         = n
    | n <= b        = firstOmittedFrom (b+1) rest
    | otherwise     = firstOmittedFrom n rest

countIncluded :: [(Int, Int)] -> Int
countIncluded [] = 0
countIncluded [(a, b)] = b - a + 1
countIncluded ((a1, b1) : (a2, b2) : rest)
    | b1 < a2           = countIncluded ((a2, b2) : rest) + b1 - a1 + 1
    | b1 > b2           = countIncluded ((a1, b1) : rest)
    | otherwise         = countIncluded ((a2, b2) : rest) + a2 - a1

getRange :: String -> (Int, Int)
getRange s = (read $ takeWhile (/= '-') s, read $ tail $ dropWhile (/= '-') s)
