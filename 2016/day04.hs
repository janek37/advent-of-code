import System.IO
import Text.Regex.TDFA
import Data.List (sort)
import Data.Char
import Data.Foldable (find)


main = do
    s <- getContents
    let realRooms = filter isReal (map parseLine (lines s))
    print (sum (map (\(n, s, c) -> s) realRooms))
    let Just (_, target, _) = find (\r -> decrypt r == "northpole object storage") realRooms
    print target

parseLine :: String -> (String, Int, String)
parseLine line =
    (encryptedName, read sectorID, checksum)
    where (_::String, _::String, _::String, [encryptedName, sectorID, checksum]) = line =~ "([a-z-]+)-([0-9]+)\\[([a-z]+)\\]"

isReal (encryptedName, _, checksum) =
    checksum == realChecksum
    where realChecksum = map snd (take 5 (sort (map (\(cnt, ch) -> (-cnt, ch)) (counts (filter (/= '-') encryptedName)))))

counts s = countsSorted (sort s)

countsSorted :: String -> [(Int, Char)]
countsSorted [] = []
countsSorted [ch] = [(1, ch)]
countsSorted (ch:ch1:s) =
    if ch == ch1
        then (cnt+1, ch):rest
        else (1, ch):(cnt, ch1):rest
    where (cnt, ch1'):rest = countsSorted (ch1:s)

decrypt (encryptedName, sectorID, _) = map (decryptChar sectorID) encryptedName

decryptChar key ch = if ch == '-' then ' ' else chr (mod (ord ch - ord 'a' + key) 26 + ord 'a')