module Day10 where

import Data.List
import Text.Regex.TDFA


day10 :: IO ()
day10 = do
    s <- getContents
    let instructions = lines s
    let valueInstructions = map parseValues $ filter (isPrefixOf "value") instructions
    let giveInstructions = map parseGives $ filter (isPrefixOf "bot") instructions
    let bots = applyValues valueInstructions
    let (completeBots, outputs) = applyGives bots giveInstructions []
    case find ((== (Just 17, Just 61)) . snd) completeBots of
        Just (botNo, _) -> print botNo
        Nothing -> return ()
    print $ product $ map snd $ filter ((< 3) . fst) outputs

data Recipient = Bot Int | Output Int

applyValues :: [(Int, Int)] -> [(Int, (Maybe Int, Maybe Int))]
applyValues instructions =
    foldl updateBots [firstBot] sorted
    where
        sorted = sort instructions
        (firstBotNo, _) = head sorted
        firstBot = (firstBotNo, (Nothing, Nothing))

updateBots :: [(Int, (Maybe Int, Maybe Int))] -> (Int, Int) -> [(Int, (Maybe Int, Maybe Int))]
updateBots bots (botNo, value) =
    if botNo == fst (head bots) then
        let botValues = snd (head bots) in
            (botNo, updateBot botValues value) : tail bots
    else
        (botNo, (Just value, Nothing)) : bots

applyGives :: [(Int, (Maybe Int, Maybe Int))] -> [(Int, Recipient, Recipient)] -> [(Int, Int)] -> ([(Int, (Maybe Int, Maybe Int))], [(Int, Int)])
applyGives bots giveInstructions outputs
    | all isComplete bots       = (bots, outputs)
    | otherwise                 =
        applyGives bots'' giveInstructions outputs'
        where
            (incompletePrefix, rest) = break isComplete bots
            firstComplete = head rest
            maybeInstruction = find (\(n, _, _) -> n == fst firstComplete) giveInstructions
            bots' = incompletePrefix ++ tail rest ++ [firstComplete]
            (bots'', outputs') = case maybeInstruction of
                Just instruction -> applyGive bots' instruction outputs
                Nothing -> ([], [])

isComplete :: (a1, (Maybe a2, Maybe a3)) -> Bool
isComplete bot =
    case bot of
        (_, (Just _, Just _)) -> True
        _ -> False

applyGive :: [(Int, (Maybe Int, Maybe Int))] -> (Int, Recipient, Recipient) -> [(Int, Int)] -> ([(Int, (Maybe Int, Maybe Int))], [(Int, Int)])
applyGive bots (botNo, low, high) outputs = (bots'', outputs'')
    where
        (lowValue, highValue) = case find ((== botNo) . fst) bots of
            Just (_, (Just lo, Just hi)) -> (lo, hi)
            _ -> (0, 0)
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
        _ -> (Nothing, Nothing)

parseValues :: String -> (Int, Int)
parseValues s =
    case matches of
        [bot, value] -> (bot, value)
        _ -> (0, 0)
    where
        matches = map read $ getAllTextMatches (s =~ "[0-9]+")

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
