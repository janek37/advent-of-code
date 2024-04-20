module Day04 where

import Text.Regex.TDFA
import Data.List (sort)
import Data.Char
import Data.Foldable (find)


day04 :: IO ()
day04 = do
    s <- getContents
    let realRooms = filter isReal (map parseLine (lines s))
    print (sum (map (\(_, sectId, _) -> sectId) realRooms))
    let maybeFound = find (\r -> decrypt r == "northpole object storage") realRooms
    case maybeFound of
        Just (_, target, _) -> print target
        Nothing -> return ()


parseLine :: String -> (String, Int, String)
parseLine line =
    (encryptedName, read sectorID, checksum)
    where
        foundMatch = line =~ "([a-z-]+)-([0-9]+)\\[([a-z]+)\\]" :: (String, String, String, [String])
        (encryptedName, sectorID, checksum) = case foundMatch of
            (_, _, _, [name, sid, check]) -> (name, sid, check)
            _ -> ("", "", "")

isReal :: (String, Int, String) -> Bool
isReal (encryptedName, _, checksum) =
    checksum == realChecksum
    where realChecksum = map snd (take 5 (sort (map (\(cnt, ch) -> (-cnt, ch)) (counts (filter (/= '-') encryptedName)))))

counts :: String -> [(Int, Char)]
counts s = countsSorted (sort s)

countsSorted :: String -> [(Int, Char)]
countsSorted [] = []
countsSorted [ch] = [(1, ch)]
countsSorted (ch:ch1:s) =
    if ch == ch1
        then (cnt+1, ch):rest
        else (1, ch):(cnt, ch1):rest
    where
        (cnt, rest) = case countsSorted (ch1:s) of
            (cnt', _):rest' -> (cnt', rest')
            _ -> (0, [])

decrypt :: (String, Int, c) -> String
decrypt (encryptedName, sectorID, _) = map (decryptChar sectorID) encryptedName

decryptChar :: Int -> Char -> Char
decryptChar key ch = if ch == '-' then ' ' else chr (mod (ord ch - ord 'a' + key) 26 + ord 'a')