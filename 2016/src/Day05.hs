module Day05 where

import Crypto.Hash.MD5
import Data.ByteString.Base16 as Base16
import Data.ByteString.UTF8 as BU
import Data.Char (isOctDigit, ord)
import Data.Foldable


day05 :: IO ()
day05 = do
    s <- getLine
    let suffixes = findSuffixes s 0
    let interestingHashes = map (BU.toString . md5digest s) suffixes
    putStrLn (map (!! 5) (Prelude.take 8 interestingHashes))
    let betterHashes = filter isBetter interestingHashes
    let digitPlaces = map digitPlace betterHashes
    putStrLn (map (digitAt digitPlaces) [0 .. 7])


fiveZeros :: ByteString
fiveZeros = BU.fromString "00000"

findSuffixes :: String -> Int -> [Int]
findSuffixes prefix n =
    if BU.take 5 digest == fiveZeros
        then n : findSuffixes prefix (n+1)
        else findSuffixes prefix (n+1)
    where
        digest = md5digest prefix n

md5digest :: String -> Int -> ByteString
md5digest prefix n = Base16.encode (hash (BU.fromString (prefix ++ show n)))

isBetter :: String -> Bool
isBetter digest = isOctDigit (digest !! 5)

digitPlace :: String -> (Char, Int)
digitPlace digest = (digest !! 6, ord (digest !! 5) - ord '0')

digitAt :: [(Char, Int)] -> Int -> Char
digitAt digitPlaces n = digit
    where
        maybeDigitPlace = find ((== n) . snd) digitPlaces
        digit = case maybeDigitPlace of
            Just (d, _) -> d
            Nothing -> '0'
