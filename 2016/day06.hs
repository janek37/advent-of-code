import System.IO
import Data.List (sort)


main = do
    s <- getContents
    let transposed = transpose (lines s)
    putStrLn (map mostCommon transposed)
    putStrLn (map leastCommon transposed)


mostCommon s = chooseSorted max 0 (sort s)
leastCommon s = chooseSorted min (maxBound :: Int) (sort s)

chooseSorted choose initial s =
    snd (foldl choose (initial, '\0') counts)
    where counts = foldl updateCounts [] s

updateCounts counts ch =
    if null counts || snd (head counts) /= ch
        then (1, ch):counts
        else (fst (head counts) + 1, ch):tail counts

transpose ls = foldr (zipWith (:)) (map (const []) (head ls)) ls
