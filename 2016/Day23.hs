module Day23 where
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

day23 :: IO ()
day23 = do
    s <- getContents
    let instructions = map (getInstruction . words) (lines s)
    print $ run instructions (Map.fromList [('a', 7)]) Map.! 'a'
    print $ run instructions (Map.fromList [('a', 12)]) Map.! 'a'

data Value = Register Char | Literal Int deriving (Eq, Show)
data Instruction = Copy Value Value | Inc Value | Dec Value | Jnz Value Value | Toggle Value deriving Show

getInstruction :: [String] -> Instruction
getInstruction parts =
    case head parts of
        "cpy" -> Copy value1 value2
        "inc" -> Inc value1
        "dec" -> Dec value1
        "jnz" -> Jnz value1 value2
        "tgl" -> Toggle value1
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
type State = ([Instruction], Int, Registers)

run :: [Instruction] -> Map.Map Char Int -> Map.Map Char Int
run instructions registers = finalRegisters
    where
        (_, _, finalRegisters) = until (not . isValidState) (fallback nextStateOptimized nextState) initialState
        initialState = (instructions, 0, registers)

nextState :: State -> State
nextState (instructions, ip, registers) =
    case instructions !! ip of
        Copy v1 v2 -> (instructions, ip+1, copy v1 v2 registers)
        Inc v1 -> (instructions, ip+1, add 1 v1 registers)
        Dec v1 -> (instructions, ip+1, add (-1) v1 registers)
        Jnz v1 v2 -> (instructions, ip + jnz v1 v2 registers, registers)
        Toggle v1 -> (toggle v1 instructions ip registers, ip+1, registers)

nextStateOptimized :: State -> Maybe State
nextStateOptimized (instructions, ip, registers) =
    case take 6 (drop ip instructions) of
        [Copy value (Register a1), Inc (Register b), Dec (Register a2), Jnz (Register a3) (Literal (-2)), Dec (Register c1), Jnz (Register c2) (Literal (-5))] ->
            if a1 == a2 && a1 == a3 && c1 == c2
                && a1 /= b && a1 /= c1 && b /= c1
                && value /= Register a1 && value /= Register b && value /= Register c1
            then Just (instructions, ip+6, add (eval value registers * eval (Register c1) registers) (Register b) registers)
            else Nothing
        _ -> Nothing

fallback :: (a -> Maybe b) -> (a -> b) -> a -> b
fallback f g x = fromMaybe (g x) (f x) 

isValidState :: State -> Bool
isValidState (instructions, ip, _) = ip >= 0 && ip < length instructions

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

toggle :: Value -> [Instruction] -> Int -> Registers -> [Instruction]
toggle value instructions ip registers = updateAt (ip + eval value registers) toggleInstruction instructions

toggleInstruction :: Instruction -> Instruction
toggleInstruction instruction =
    case instruction of
        Copy v1 v2 -> Jnz v1 v2
        Inc v1 -> Dec v1
        Dec v1 -> Inc v1
        Jnz v1 v2 -> Copy v1 v2
        Toggle v1 -> Inc v1

eval :: Value -> Registers -> Int
eval (Literal n) = const n
eval (Register name) = fromMaybe 0 . Map.lookup name

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt _ _ [] = []
updateAt 0 f (x:xs) = f x : xs
updateAt n f (x:xs) = x : updateAt (n-1) f xs
