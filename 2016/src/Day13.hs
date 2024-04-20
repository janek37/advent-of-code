module Day13 where

import System.IO
import qualified Data.Set as Set
import Data.List (findIndex)


day13 = do
    s <- getLine
    let favNumber = read s :: Int
    let layers = getLayers 1 1 (isOpenSpace favNumber)
    let Just steps = findIndex ((31, 39) `Set.member`) layers
    print steps
    print $ sum $ map Set.size (take 51 layers)

magicFormula x y = x*x + 3*x + 2*x*y + y + y*y

isOpenSpace favNumber x y = even $ countBinaryOnes (magicFormula x y + favNumber)

countBinaryOnes n
    | n == 0        = 0
    | even n        = countBinaryOnes (n `div` 2)
    | odd n         = 1 + countBinaryOnes (n `div` 2)

getLayers :: Int -> Int -> (Int -> Int -> Bool) -> [Set.Set (Int, Int)]
getLayers x0 y0 isOpen = layers'
    where
        layers' = Set.singleton (x0, y0) : map nextLayer (zip layers' (Set.empty : layers'))
        nextLayer (pairs, prevPairs) = (Set.unions . map (uncurry $ neighbors isOpen) $ Set.elems pairs) `Set.difference` prevPairs

neighbors :: (Int -> Int -> Bool) -> Int -> Int -> Set.Set (Int, Int)
neighbors isOpen x y = Set.fromList [(x', y') | (x', y') <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)], x' >= 0, y' >= 0, isOpen x' y']
