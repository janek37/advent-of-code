module Day18 where

day18 :: IO ()
day18 = do
    s <- getLine
    let firstRow = map charToTile s
    print $ countSafe 40 firstRow
    print $ countSafe 400000 firstRow


data Tile = Safe | Trap deriving (Eq, Show)

countSafe :: Int -> [Tile] -> Int
countSafe n firstRow = sum [1::Int | row <- rows n firstRow, tile <- row, tile == Safe]

rows :: Int -> [Tile] -> [[Tile]]
rows n firstRow = take n $ iterate nextRow firstRow

nextRow :: [Tile] -> [Tile]
nextRow row = zipWith (\a b -> if a == b then Safe else Trap) (Safe:row) (tail row ++ [Safe])

charToTile :: Char -> Tile
charToTile ch = if ch == '.' then Safe else Trap
