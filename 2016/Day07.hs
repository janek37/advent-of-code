module Day07 where

import Data.List (isInfixOf)


day07 :: IO ()
day07 = do
    s <- getContents
    let ips = lines s
    print (sum (map (fromEnum . isTLS) ips))
    print (sum (map (fromEnum . isSSL) ips))


isTLS :: [Char] -> Bool
isTLS ip = not (any isABBA hypernets) && any isABBA supernets
    where (supernets, hypernets) = splitIP ip

isSSL :: [Char] -> Bool
isSSL ip =
    any (\bab -> any (isInfixOf bab) hypernets) babs
    where
        (supernets, hypernets) = splitIP ip
        babs = map abToBab (concatMap findABAs supernets)

abToBab :: [a] -> [a]
abToBab [a, b] = [b, a, b]
abToBab _ = []

isABBA :: Eq a => [a] -> Bool
isABBA s = any startsWithABBA (tails s)

startsWithABBA :: Eq a => [a] -> Bool
startsWithABBA s =
    case s of
        ch1:ch2:ch3:ch4:_ -> ch1 == ch4 && ch2 == ch3 && ch1 /= ch2
        _ -> False

findABAs :: Eq a => [a] -> [[a]]
findABAs s = map (take 2) (filter startsWithABA (tails s))

startsWithABA :: Eq a => [a] -> Bool
startsWithABA s =
    case s of
        ch1:ch2:ch3:_ -> ch1 == ch3 && ch1 /= ch2
        _ -> False


tails :: [a] -> [[a]]
tails [] = [[]]
tails l = l : tails (tail l)

splitIP :: [Char] -> ([[Char]], [[Char]])
splitIP "" = ([], [])
splitIP (ch:s) =
    if ch == '[' then
        let
            (hypernet, rest) = span (/= ']') s
            (supernets, hypernets) = splitIP (tail rest)
        in
            (supernets, hypernet:hypernets)
    else
        let
            (supernet, rest) = span (/= '[') (ch:s)
            (supernets, hypernets) = splitIP rest
        in
            (supernet:supernets, hypernets)
