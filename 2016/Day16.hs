module Day16 where
import Data.Bits (shiftR)

day16 :: IO ()
day16 = do
    s <- getLine
    putStrLn $ checksum 272 $ modifiedDragonCurve s
    putStrLn $ checksum 35651584 $ modifiedDragonCurve s

modifiedDragonCurve :: String -> String
modifiedDragonCurve s = concat [s ++ [a] ++ s' ++ [b] | (a, b) <- pairs dragonCurve]
    where s' = inreverse s

dragonCurve :: String
dragonCurve = '0' : concat ['0' : inreverse (take (2^n - 1) dragonCurve) | n :: Int <- [1..]]

inreverse :: String -> String
inreverse = inverse . reverse

inverse :: String -> String
inverse s = [if ch == '0' then '1' else '0' | ch <- s]

checksum :: Int -> String -> String
checksum n = take (n `shiftR` k) . nTimes k reduce
    where k = maxPowerOfTwo n

maxPowerOfTwo :: Int -> Int
maxPowerOfTwo n = if odd n then 0 else 1 + maxPowerOfTwo (n `div` 2)

reduce :: String -> String
reduce s = [if ch1 == ch2 then '1' else '0' | (ch1, ch2) <- pairs s]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (a:b:t) = (a, b) : pairs t

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f = foldl1 (.) $ replicate n f
