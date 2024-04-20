module Day08 where

import System.IO
import Text.Regex.TDFA


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
    | s =~ "rotate column x=[0-9]+ by [0-9]+"   = parseRotateColumn s

parseRect s =
    Rect (read width, read height)
    where [width, height] = getAllTextMatches (s =~ "[0-9]+")

parseRotateRow s =
    RotateRow (read y, read offset)
    where [y, offset] = getAllTextMatches (s =~ "[0-9]+")

parseRotateColumn s =
    RotateColumn (read x, read offset)
    where [x, offset] = getAllTextMatches (s =~ "[0-9]+")

getPixel operations (x, y) = isOn
    where (_, _, isOn) = foldr applyOperation (x, y, False) operations

applyOperation :: Operation -> (Int, Int, Bool) -> (Int, Int, Bool)
applyOperation op (x, y, result) =
    case op of
        Rect (width, height) -> if x < width && y < height then (x, y, True) else (x, y, result)
        RotateRow (rowY, offset) -> if y == rowY then ((x - offset) `mod` 50, y, result) else (x, y, result)
        RotateColumn (colX, offset) -> if x == colX then (x, (y - offset) `mod` 6, result) else (x, y, result)
