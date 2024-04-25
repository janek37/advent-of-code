module Day21 where
import GHC.OldList (elemIndex)
import Data.Maybe (fromMaybe)
import Text.Regex.TDFA

day21 :: IO ()
day21 = do
    s <- getContents
    let ops = map getOperation $ lines s
    putStrLn $ foldl perform "abcdefgh" ops
    putStrLn $ foldr unperform "fbgdceah" ops


data Operation = SwapPos Int Int | SwapChar Char Char | Rotate Int | RotatePos Char | Reverse Int Int | Move Int Int deriving Show

getOperation :: String -> Operation
getOperation s
    | s =~ "swap position [0-9]+ with position [0-9]+"      = SwapPos i1 i2
    | s =~ "swap letter . with letter ."                = SwapChar ch1 ch2
    | s =~ "rotate left [0-9]+ step"                      = Rotate i1
    | s =~ "rotate right [0-9]+ step"                     = Rotate (-i1)
    | s =~ "rotate based on position of letter ."       = RotatePos ch1
    | s =~ "reverse positions [0-9]+ through [0-9]+"        = Reverse i1 i2
    | s =~ "move position [0-9]+ to position [0-9]+"        = Move i1 i2
    | otherwise                                         = Rotate 0
    where
        i1 = read (s =~ "[0-9]+")
        i2 = nums !! 1
        ch1 = head (head chars)
        ch2 = head (chars !! 1)
        nums = map read $ getAllTextMatches (s =~ "[0-9]+")
        chars = getAllTextMatches (s =~ "\\b[^ ]\\b")

perform :: String -> Operation -> String
perform s op = case op of
    SwapPos a b -> swapPos a b s
    SwapChar ch1 ch2 -> swapChar ch1 ch2 s
    Rotate n -> rotate n s
    RotatePos ch -> rotatePos ch s
    Reverse a b -> reversePart a b s
    Move a b -> move a b s

unperform :: Operation -> String -> String
unperform op s = case op of
    SwapPos a b -> swapPos a b s
    SwapChar ch1 ch2 -> swapChar ch1 ch2 s
    Rotate n -> rotate (-n) s
    RotatePos ch -> unrotatePos ch s
    Reverse a b -> reversePart a b s
    Move a b -> move b a s

swapPos :: Int -> Int -> String -> String
swapPos a b s = take a' s ++ [s !! b'] ++ drop (a'+1) (take b' s) ++ [s !! a'] ++ drop (b'+1) s
    where
        a' = min a b
        b' = max a b

swapChar :: Char -> Char -> String -> String
swapChar ch1 ch2 s = swapPos pos1 pos2 s
    where
        pos1 = fromMaybe 0 $ elemIndex ch1 s
        pos2 = fromMaybe 0 $ elemIndex ch2 s

rotate :: Int -> String -> String
rotate n s = drop n' s ++ take n' s
    where n' = n `mod` length s

rotatePos :: Char -> String -> String
rotatePos ch s = rotate (-n) s
    where
        n = if ind < 4 then ind + 1 else ind + 2
        ind = fromMaybe 0 $ elemIndex ch s

-- works for strings of length 8
unrotatePos :: Char -> String -> String
unrotatePos ch s = rotate n s
    where
        n =
            if even ind then
                if ind == 0 then 1 else ind `div` 2 - 3
            else
                ind `div` 2 + 1
        ind = fromMaybe 0 $ elemIndex ch s

reversePart :: Int -> Int -> String -> String
reversePart a b s = take a s ++ reverse (drop a (take (b+1) s)) ++ drop (b+1) s

move :: Int -> Int -> String -> String
move a b s =
    if a < b then
        take a s ++ drop (a+1) (take (b+1) s) ++ [s !! a] ++ drop (b+1) s
    else
        take b s ++ [s !! a] ++ drop b (take a s) ++ drop (a+1) s