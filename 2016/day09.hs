import System.IO
import Data.Char


main = do
    s <- getContents
    let file = filter (not . isSpace) s
    print $ decompressedLength parser1 file
    print $ decompressedLength parser2 file


decompressedLength :: (String -> (Int, String)) -> String -> Int
decompressedLength parser "" = 0
decompressedLength parser s
    | head s == '('     = let (parsed, rest) = parser $ tail s in parsed + decompressedLength parser rest
    | otherwise         = 1 + decompressedLength parser (tail s)

parser1 :: String -> (Int, String)
parser1 s =
    (count * len, rest)
    where (count, len, _, rest) = parseMarker s


parser2 :: String -> (Int, String)
parser2 s =
    (count * decompressedLength parser2 substring, rest)
    where (count, _, substring, rest) = parseMarker s

parseMarker :: String -> (Int, Int, String, String)
parseMarker s =
    (count, len, substring, rest'')
    where
        (lenStr, rest) = span isDigit s
        len = read lenStr
        (countStr, rest') = span isDigit $ tail rest  -- skip 'x'
        count = read countStr
        (substring, rest'') = splitAt len $ tail rest'  -- skip ')'
