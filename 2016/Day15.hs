module Day15 where

import Text.Regex.TDFA


day15 :: IO ()
day15 = do
    s <- getContents
    let discs = map parseLine (lines s)
    print $ findChineseRemainder $ discsToRemainders discs
    print $ findChineseRemainder $ discsToRemainders (discs ++ [(11, 0)])


discsToRemainders :: [(Int, Int)] -> [(Int, Int)]
discsToRemainders = zipWith (\t (a, b) -> (a, (-b - t) `mod` a)) [1..]

parseLine :: String -> (Int, Int)
parseLine s =
    case matches of
        [_, size, _, position] -> (read size, read position)
        _ -> (0, 0)
    where matches = getAllTextMatches (s =~ "[0-9]+")

findChineseRemainder :: [(Int, Int)] -> Int
findChineseRemainder modRems = case modRems of
    [] -> 0
    [(_, r)] -> r
    [(m1, r1), (m2, r2)] -> let (a, b) = extendedEuclid m1 m2 in (r2 * a*m1 + r1 * b*m2) `mod` (m1*m2)
    h : t -> findChineseRemainder [h, (product (map fst t), findChineseRemainder t)] `mod` product (map fst modRems)

-- find m, n such that m*a + n*b = gcd
extendedEuclid :: Int -> Int -> (Int, Int)
extendedEuclid a b
    | a `mod` b == 0    = (0, 1)
    | b `mod` a == 0    = (1, 0)
    | otherwise         = if a == a' then (m, n) else (n, m)
        where
            (m', n) = extendedEuclid a' (b' `mod` a')
            m = m' - (b' `div` a') * n
            a' = min a b
            b' = max a b
