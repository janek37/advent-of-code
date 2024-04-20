module Day08 where

import Text.Regex.TDFA


day08 :: IO ()
day08 = do
    s <- getContents
    let operations = map parseOperation (lines s)
    print (sum ([(fromEnum . getPixel operations) (x, y) | x <- [0 .. 49], y <- [0 .. 5]]))
    mapM_ putStrLn [[if getPixel operations (x, y) then '#' else ' ' | x <- [0 .. 49]] | y <- [0 .. 5]]


data Operation = Rect (Int, Int) | RotateRow (Int, Int) | RotateColumn (Int, Int)

parseOperation :: String -> Operation
parseOperation s
    | s =~ "rect [0-9]+x[0-9]+"                 = parseRect s
    | s =~ "rotate row y=[0-9]+ by [0-9]+"      = parseRotateRow s
    | otherwise                                 = parseRotateColumn s

parseRect :: String -> Operation
parseRect s =
    Rect (read width, read height)
    where
        matches = getAllTextMatches (s =~ "[0-9]+")
        (width, height) = case matches of
            [w, h] -> (w, h)
            _ -> ("", "")

parseRotateRow :: String -> Operation
parseRotateRow s =
    RotateRow (read y, read offset)
    where
        matches = getAllTextMatches (s =~ "[0-9]+")
        (y, offset) = case matches of
            [y', off] -> (y', off)
            _ -> ("", "")

parseRotateColumn :: String -> Operation
parseRotateColumn s =
    RotateColumn (read x, read offset)
    where
        matches = getAllTextMatches (s =~ "[0-9]+")
        (x, offset) = case matches of
            [x', off] -> (x', off)
            _ -> ("", "")

getPixel :: [Operation] -> (Int, Int) -> Bool
getPixel operations (x, y) = isOn
    where (_, _, isOn) = foldr applyOperation (x, y, False) operations

applyOperation :: Operation -> (Int, Int, Bool) -> (Int, Int, Bool)
applyOperation op (x, y, result) =
    case op of
        Rect (width, height) -> if x < width && y < height then (x, y, True) else (x, y, result)
        RotateRow (rowY, offset) -> if y == rowY then ((x - offset) `mod` 50, y, result) else (x, y, result)
        RotateColumn (colX, offset) -> if x == colX then (x, (y - offset) `mod` 6, result) else (x, y, result)
