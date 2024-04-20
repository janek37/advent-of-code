module Day03 where

import System.IO


day03 = do
    s <- getContents
    let parsed = map (map read . words) (lines s)
    print (count isTriangle parsed)
    print (count isTriangle (concatMap columns (triples parsed)))


count p = sum . map (fromEnum . p)

isTriangle [a, b, c] = 2 * max a (max b c) < a + b + c

triples l = case l of
    [] -> []
    [x] -> []
    [x1, x2] -> []
    (x1:x2:x3:xs) -> (x1, x2, x3):triples xs

columns ([a1, a2, a3], [b1, b2, b3], [c1, c2, c3]) = [[a1, b1, c1], [a2, b2, c2], [a3, b3, c3]]
