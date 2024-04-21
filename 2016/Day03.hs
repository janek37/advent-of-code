module Day03 where


day03 :: IO ()
day03 = do
    s <- getContents
    let parsed = map (map read . words) (lines s)
    print (count isTriangle parsed)
    print (count isTriangle (concatMap columns (triples parsed)))


count :: (a -> Bool) -> [a] -> Int
count p = sum . map (fromEnum . p)

isTriangle :: [Int] -> Bool
isTriangle [a, b, c] = 2 * max a (max b c) < a + b + c
isTriangle _ = False

triples :: [c] -> [(c, c, c)]
triples l = case l of
    [] -> []
    [_] -> []
    [_, _] -> []
    (x1:x2:x3:xs) -> (x1, x2, x3):triples xs

columns :: ([a], [a], [a]) -> [[a]]
columns ([a1, a2, a3], [b1, b2, b3], [c1, c2, c3]) = [[a1, b1, c1], [a2, b2, c2], [a3, b3, c3]]
columns _ = []
