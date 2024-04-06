import System.IO
import Data.List (isInfixOf)


main = do
    s <- getContents
    let ips = lines s
    print (sum (map (fromEnum . isTLS) ips))
    print (sum (map (fromEnum . isSSL) ips))


isTLS ip = not (any isABBA hypernets) && any isABBA supernets
    where (supernets, hypernets) = splitIP ip

isSSL ip =
    any (\bab -> any (isInfixOf bab) hypernets) babs
    where
        (supernets, hypernets) = splitIP ip
        babs = map (\[a, b] -> [b, a, b]) (concatMap findABAs supernets)

isABBA s = any startsWithABBA (tails s)

startsWithABBA s =
    case s of
        ch1:ch2:ch3:ch4:rest -> ch1 == ch4 && ch2 == ch3 && ch1 /= ch2
        _ -> False

findABAs s = map (take 2) (filter startsWithABA (tails s))

startsWithABA s =
    case s of
        ch1:ch2:ch3:rest -> ch1 == ch3 && ch1 /= ch2
        _ -> False


tails [] = [[]]
tails l = l : tails (tail l)

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
