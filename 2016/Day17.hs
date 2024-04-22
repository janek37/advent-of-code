module Day17 where

import qualified Data.Map as Map
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.UTF8 as BU
import Data.Foldable (find)
import Data.Maybe (fromMaybe)

day17 :: IO ()
day17 = do
    s <- getLine
    putStrLn $ findShortestPath s
    print $ maximum $ map length $ findAllPaths s


findShortestPath :: String -> String
findShortestPath prefix = pathToString $ fromMaybe [] path
    where
        paths = until (any isGoal) (extendPaths prefix) [[]]
        path = find isGoal paths
        isGoal = (== (3, 3)) . currentLocation

findAllPaths :: String -> [[(Int, Int)]]
findAllPaths prefix = findAllPathsFrom []
    where
        findAllPathsFrom path =
            if currentLocation path == (3, 3) then [path]
            else concatMap findAllPathsFrom (extendPath prefix path)

extendPaths :: String -> [[(Int, Int)]] -> [[(Int, Int)]]
extendPaths prefix = concatMap $ extendPath prefix

extendPath :: String -> [(Int, Int)] -> [[(Int, Int)]]
extendPath prefix path = [path ++ [dir] | dir <- openDoors prefix path]

openDoors :: String -> [(Int, Int)] -> [(Int, Int)]
openDoors prefix path = [(dx, dy) | (dx, dy) <- openDirections, x + dx `elem` [0..3], y + dy `elem` [0..3]]
    where
        hash = md5digest prefix path
        openDirections = [dir | (dir, ch) <- zip directions (BU.toString $ BU.take 4 hash), ch > 'a']
        (x, y) = currentLocation path

currentLocation :: [(Int, Int)] -> (Int, Int)
currentLocation path = (sum (map fst path), sum (map snd path))

pathToString :: [(Int, Int)] -> String
pathToString = map (directionChars Map.!)

directionChars :: Map.Map (Int, Int) Char
directionChars = Map.fromList $ zip directions "UDLR"

directions :: [(Int, Int)]
directions = [(0, -1), (0, 1), (-1, 0), (1, 0)]

md5digest :: String -> [(Int, Int)] -> BU.ByteString
md5digest prefix path = Base16.encode (MD5.hash (BU.fromString (prefix ++ pathToString path)))
