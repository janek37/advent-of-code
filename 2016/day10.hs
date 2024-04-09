import System.IO
import Data.List
import Text.Regex.TDFA
import Control.Arrow (Arrow(first))


main = do
    s <- getContents
    let instructions = lines s
    let valueInstructions = map parseValues $ filter (isPrefixOf "value") instructions
    let giveInstructions = map parseGives $ filter (isPrefixOf "bot") instructions
    let bots = applyValues valueInstructions
    let (completeBots, outputs) = applyGives bots giveInstructions []
    let Just (botNo, _) = find ((== (Just 17, Just 61)) . snd) completeBots
    print botNo
    print $ product $ map snd $ filter ((< 3) . fst) outputs

data Recipient = Bot Int | Output Int

applyValues :: [(Int, Int)] -> [(Int, (Maybe Int, Maybe Int))]
applyValues instructions =
    foldl updateBots [firstBot] sorted
    where
        sorted = sort instructions
        (firstBotNo, firstValue) = head sorted
        firstBot = (firstBotNo, (Nothing, Nothing))

updateBots :: [(Int, (Maybe Int, Maybe Int))] -> (Int, Int) -> [(Int, (Maybe Int, Maybe Int))]
updateBots bots (botNo, value) =
    if botNo == fst (head bots) then
        let botValues = snd (head bots) in
            (botNo, updateBot botValues value) : tail bots
    else
        (botNo, (Just value, Nothing)) : bots

applyGives bots giveInstructions outputs
    | all isComplete bots       = (bots, outputs)
    | otherwise                 =
        applyGives bots'' giveInstructions outputs'
        where
            (incompletePrefix, rest) = break isComplete bots
            firstComplete = head rest
            Just instruction = find (\(n, _, _) -> n == fst firstComplete) giveInstructions
            bots' = incompletePrefix ++ tail rest ++ [firstComplete]
            (bots'', outputs') = applyGive bots' instruction outputs

isComplete bot =
    case bot of
        (_, (Just _, Just _)) -> True
        _ -> False

applyGive bots (botNo, low, high) outputs = (bots'', outputs'')
    where
        Just (_, (Just lowValue, Just highValue)) = find ((== botNo) . fst) bots
        update bots_ n value =
            if any ((== n) . fst) bots_ then
                map (\(m, b) -> if m == n then (n, updateBot b value) else (m, b)) bots_
            else
                (n, (Just value, Nothing)) : bots_
        (bots', outputs') = case low of
            Bot b -> (update bots b lowValue, outputs)
            Output n -> (bots, (n, lowValue):outputs)
        (bots'', outputs'') = case high of
            Bot b -> (update bots' b highValue, outputs')
            Output n -> (bots', (n, highValue):outputs')

updateBot :: (Maybe Int, Maybe Int) -> Int -> (Maybe Int, Maybe Int)
updateBot bot value =
    case bot of
        (Nothing, Nothing) -> (Just value, Nothing)
        (Just v, Nothing) -> (Just $ min v value, Just $ max v value)

parseValues :: String -> (Int, Int)
parseValues s =
    (bot, value)
    where [value, bot] = map read $ getAllTextMatches (s =~ "[0-9]+")

parseGives :: String -> (Int, Recipient, Recipient)
parseGives s =
    (bot, low, high)
    where
        (_::String, _::String, _::String, submatches) = s =~ "bot ([0-9]+) gives low to (bot|output) ([0-9]+) and high to (bot|output) ([0-9]+)"
        bot = read $ head submatches
        lowNo = read $ submatches !! 2
        highNo = read $ submatches !! 4
        low = if (submatches !! 1) == "bot" then Bot lowNo else Output lowNo
        high = if (submatches !! 3) == "bot" then Bot highNo else Output highNo
