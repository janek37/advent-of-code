module Day25 where
import qualified Data.Map.Strict as Map
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.List (find)

day25 :: IO ()
day25 = do
    s <- getContents
    let instructions = map (getInstruction . words) (lines s)
    print $ findCorrectInput instructions

data Value = Register Char | Literal Int deriving Eq
data Instruction = Copy Value Value | Inc Value | Dec Value | Jnz Value Value | Out Value

getInstruction :: [String] -> Instruction
getInstruction parts =
    case head parts of
        "cpy" -> Copy value1 value2
        "inc" -> Inc value1
        "dec" -> Dec value1
        "jnz" -> Jnz value1 value2
        "out" -> Out value1
        _ -> error "wrong instruction code"
    where
        value1 = head values
        value2 = values !! 1
        values = map getValue $ tail parts

getValue :: String -> Value
getValue "" = error "invalid argument"
getValue (ch:s) =
    if isDigit ch || ch == '-' then
        Literal (read (ch:s))
    else
        Register ch

type Registers = Map.Map Char Int
type State = (Int, Registers, [Int])

findCorrectInput :: [Instruction] -> Int
findCorrectInput instructions = fromMaybe 0 $ find (isCorrectOutput . getOutput instructions) [1..]

getOutput :: [Instruction] -> Int -> [Int]
getOutput instructions a = run instructions (Map.fromList [('a', a)])

isCorrectOutput :: [Int] -> Bool
isCorrectOutput l = not (null l) && even (length l) && all (uncurry (==)) (zip l (cycle [1, 0]))

run :: [Instruction] -> Map.Map Char Int -> [Int]
run instructions registers = output
    where
        (_,  _, output) = until (not . isValidState instructions) (fallback (nextStateOptimized instructions) (nextState instructions)) initialState
        initialState = (0, registers, [])

nextState :: [Instruction] -> State -> State
nextState instructions (ip, registers, output) =
    case instructions !! ip of
        Copy v1 v2 -> (ip+1, copy v1 v2 registers, output)
        Inc v1 -> (ip+1, add 1 v1 registers, output)
        Dec v1 -> (ip+1, add (-1) v1 registers, output)
        Jnz v1 v2 -> (ip + jnz v1 v2 registers, registers, output)
        Out v1 -> (ip+1, registers, eval v1 registers : output)

nextStateOptimized :: [Instruction] -> State -> Maybe State
nextStateOptimized instructions (ip, registers, output) =
    case take 6 (drop ip instructions) of
        [Copy value (Register a1), Inc (Register b), Dec (Register a2), Jnz (Register a3) (Literal (-2)), Dec (Register c1), Jnz (Register c2) (Literal (-5))] ->
            if a1 == a2 && a1 == a3 && c1 == c2
                && a1 /= b && a1 /= c1 && b /= c1
                && value /= Register a1 && value /= Register b && value /= Register c1
            then Just (ip+6, add (eval value registers * eval (Register c1) registers) (Register b) registers, output)
            else Nothing
        _ -> Nothing

fallback :: (a -> Maybe b) -> (a -> b) -> a -> b
fallback f g x = fromMaybe (g x) (f x) 

-- a little hack to not fall in infinite loop
isValidState :: [Instruction] -> State -> Bool
isValidState instructions (ip, _, _) = ip >= 0 && ip < length instructions - 1

copy :: Value -> Value -> Registers -> Registers
copy v1 v2 registers =
    case v2 of
        Literal _ -> registers
        Register name -> Map.insert name (eval v1 registers) registers

add :: Int -> Value -> Registers -> Registers
add delta value registers =
    case value of
        Literal _ -> registers
        Register name -> Map.insert name (eval value registers + delta) registers

jnz :: Value -> Value -> Registers -> Int
jnz v1 v2 registers = if eval v1 registers /= 0 then eval v2 registers else 1

eval :: Value -> Registers -> Int
eval (Literal n) = const n
eval (Register name) = fromMaybe 0 . Map.lookup name
