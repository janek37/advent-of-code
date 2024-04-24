module Day19 where
import GHC.Float (int2Float)

day19 :: IO ()
day19 = do
    s <- getLine
    let n = read s :: Int
    print $ josephus n
    print $ shiftedAcrossJosephus n


josephus :: Int -> Int
josephus n = (n - maxPowerOfTwo n) * 2 + 1

maxPowerOfTwo :: Int -> Int
maxPowerOfTwo n = 2 ^ floorLog
    where floorLog = floor (logBase 2 $ int2Float n) :: Int

shiftedAcrossJosephus :: Int -> Int
shiftedAcrossJosephus n = (acrossJosephus (n - 1) + n `div` 2 + 2) `mod` n

acrossJosephus :: Int -> Int
acrossJosephus n
    | n < 4     = 0
    | n == 4    = 3
    | otherwise = 3 * case n `mod` 3 of
        0 -> acrossJosephus (n `div` 3)
        1 -> acrossJosephus (n `div` 3 - 1) + 2
        _ -> acrossJosephus (n `div` 3) + 1
