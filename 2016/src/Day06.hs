module Day06 where

import Data.List (sort)


day06 :: IO ()
day06 = do
    s <- getContents
    let transposed = transpose (lines s)
    putStrLn (map mostCommon transposed)
    putStrLn (map leastCommon transposed)


mostCommon :: [Char] -> Char
mostCommon s = chooseSorted max 0 (sort s)
leastCommon :: [Char] -> Char
leastCommon s = chooseSorted min (maxBound :: Int) (sort s)

chooseSorted :: ((Int, Char) -> (Int, Char) -> (Int, Char)) -> Int -> [Char] -> Char
chooseSorted choose initial s =
    snd (foldl choose (initial, '\0') counts)
    where counts = foldl updateCounts [] s

updateCounts :: [(Int, Char)] -> Char -> [(Int, Char)]
updateCounts counts ch =
    if null counts || snd (head counts) /= ch
        then (1, ch):counts
        else (fst (head counts) + 1, ch):tail counts

transpose :: [[a]] -> [[a]]
transpose ls = foldr (zipWith (:)) (map (const []) (head ls)) ls
