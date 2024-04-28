module Day24 where
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.List (permutations)

day24 :: IO ()
day24 = do
    s <- getContents
    let (chart, start, checkpoints) = getChart (lines s)
    print $ findBestDistance False chart start checkpoints
    print $ findBestDistance True chart start checkpoints


getChart :: [String] -> ([[Bool]], (Int, Int), [(Int, Int)])
getChart rows = (chart, start, checkpoints)
    where
        chart = [[ch /= '#' | ch <- row] | row <- rows]
        start = head [(x, y) | (y, row) <- enumerated, (x, ch) <- row, ch == '0']
        checkpoints = [(x, y) | (y, row) <- enumerated, (x, ch) <- row, ch /= '0', isDigit ch]
        enumerated = zip [0..] (map (zip [0..]) rows)

findBestDistance :: Bool -> [[Bool]] -> (Int, Int) -> [(Int, Int)] -> Int
findBestDistance loop chart start checkpoints =
    minimum [getPermutationDistance p | p <- permutations [0..6]]
    where
        getPermutationDistance p = initialDistances !! head p + sum [distances !! i !! j | (i, j) <- pairs p] + if loop then initialDistances !! last p else 0
        initialDistances = getDistances chart start checkpoints
        distances = [getDistances chart checkpoint checkpoints | checkpoint <- checkpoints]
        pairs l = zip l (tail l)

getDistances :: [[Bool]] -> (Int, Int) -> [(Int, Int)] -> [Int]
getDistances chart origin = map (distanceMap Map.!)
    where distanceMap = getDistanceMap chart origin

type DistanceMap = Map.Map (Int, Int) Int

getDistanceMap :: [[Bool]] -> (Int, Int) -> DistanceMap
getDistanceMap chart origin = fst $ until (Seq.null . snd) (growMap chart) (Map.empty, Seq.fromList [origin])

growMap :: [[Bool]] -> (DistanceMap, Seq.Seq (Int, Int)) -> (DistanceMap, Seq.Seq (Int, Int))
growMap chart (dMap, queue) =
    case Seq.viewl queue of
        Seq.EmptyL -> (dMap, queue)
        next Seq.:< rest -> if next `Map.member` dMap then (dMap, rest) else (Map.insert next distance dMap, foldl (Seq.|>) rest neighbors)
            where
                neighbors = getNeighbors chart next
                chartedNeighbors = filter (`Map.member` dMap) neighbors
                distance = if null chartedNeighbors then 0 else 1 + minimum [dMap Map.! neighbor | neighbor <- chartedNeighbors]

getNeighbors :: [[Bool]] -> (Int, Int) -> [(Int, Int)]
getNeighbors chart (x, y) = filter (chartAt chart) [(x + dx, y + dy) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]

chartAt :: [[Bool]] -> (Int, Int) -> Bool
chartAt chart (x, y) = chart !! y !! x
