module Day14 where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.UTF8 as BU
import Data.Foldable (find)
import Control.Monad (join)
import Data.Maybe (mapMaybe)

day14 :: IO ()
day14 = do
    salt <- getLine
    let saltedHashes = hashes salt
    let tripleDigits = repeatedDigits 3 saltedHashes
    let quintupleDigits = repeatedDigits 5 saltedHashes
    print (findValidIndexes tripleDigits quintupleDigits !! 63)
    let stretchedHashes = stretchHashes salt
    let tripleDigits' = repeatedDigits 3 stretchedHashes
    let quintupleDigits' = repeatedDigits 5 stretchedHashes
    print (findValidIndexes tripleDigits' quintupleDigits' !! 63)


findValidIndexes :: [(Int, Char)] -> [(Int, Char)] -> [Int]
findValidIndexes tripleIndexes quintupleIndexes = [n | (n, ch) <- tripleIndexes, findInRange ch (n+1) (n+1000) quintupleIndexes]


findInRange :: Char -> Int -> Int -> [(Int, Char)] -> Bool
findInRange ch from to pairs = any ((== ch) . snd) pairsInRange
    where pairsInRange = takeWhile ((<= to) . fst) (dropWhile ((< from) . fst) pairs)


repeatedDigits :: Int -> [BU.ByteString] -> [(Int, Char)]
repeatedDigits k xs = mapMaybe (\(n, hash) -> (n,) <$> findRepeated k hash) $ zip [0..] xs


findRepeated :: Int -> BU.ByteString -> Maybe Char
findRepeated k bs = case join repeatedDigit of
        Just (ch, _) -> Just ch
        Nothing -> Nothing
    where
        repeatedIndex = find (\n -> BU.take k (BU.drop n bs) `elem` repeats k) [0..29]
        repeatedDigit = fmap (\ind -> BU.decode $ BU.drop ind bs) repeatedIndex

repeats :: Int -> [BU.ByteString]
repeats n = map (nDigits n) (['0' .. '9'] ++ ['a' .. 'f'])

nDigits :: Int -> Char -> BU.ByteString
nDigits n = BU.fromString . replicate n

hashes :: String -> [BU.ByteString]
hashes salt = hashes' 0
    where hashes' n = md5digest salt n : hashes' (n+1)

stretchHashes :: String -> [BU.ByteString]
stretchHashes salt = hashes' 0
    where hashes' n = stretchDigest salt n : hashes' (n+1)

md5digest :: String -> Int -> BU.ByteString
md5digest prefix n = Base16.encode (MD5.hash (BU.fromString (prefix ++ show n)))

stretchDigest :: String -> Int -> BU.ByteString
stretchDigest prefix n = stretchDigest' (BU.fromString (prefix ++ show n))
    where stretchDigest' = nTimes (2017::Int) (Base16.encode . MD5.hash)

nTimes :: (Eq t1, Num t1) => t1 -> (t2 -> t2) -> t2 -> t2
nTimes n f x
    | n == 0        = x
    | otherwise     = f (nTimes (n-1) f x)
