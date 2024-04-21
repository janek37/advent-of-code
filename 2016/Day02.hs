module Day02 where

import Data.Char


day02 :: IO ()
day02 = do
    s <- getContents
    putStrLn (code (words s))
    putStrLn (code2 (words s))

code :: [String] -> String
code = map (\n -> chr (n + 48)) . tail . scanl (foldl move) 5


move :: Int -> Char -> Int
move n 'U'
    | n > 3 = n - 3
    | otherwise = n
move n 'D'
    | n < 7 = n + 3
    | otherwise = n
move n 'L'
    | mod n 3 == 1 = n
    | otherwise = n - 1
move n 'R'
    | mod n 3 == 0 = n
    | otherwise = n + 1
move n _ = n


keypad :: [String]
keypad =
   ["  1  ",
    " 234 ",
    "56789",
    " ABC ",
    "  D  "]

code2 :: [[Char]] -> [Char]
code2 = map (\(x, y) -> keypad !! y !! x) . tail . scanl (foldl move2) (0, 2)

move2 :: (Int, Int) -> Char -> (Int, Int)
move2 pos dir =
    if newX < 0 || newX > 4 || newY < 0 || newY > 4 || keypad !! newY !! newX == ' '
        then pos
        else (newX, newY)
    where (newX, newY) = move2' pos dir

move2' :: (Int, Int) -> Char -> (Int, Int)
move2' (x, y) 'U' = (x, y-1)
move2' (x, y) 'D' = (x, y+1)
move2' (x, y) 'L' = (x-1, y)
move2' (x, y) 'R' = (x+1, y)
move2' p _ = p
