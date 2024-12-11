module AOCDay11 (day11) where

import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T

data Rule = R1 | R2 | R3
    deriving (Show, Eq)

selectRule :: Int -> (Int, Rule)
selectRule !x
    | x == 0 = (x, R1)
    | x /= 0 && isNumberOfDigitsEven x = (x, R2)
    | otherwise = (x, R3)
  where
    isNumberOfDigitsEven x' =
        let !tmp :: Float = logBase 10 $ fromIntegral x'
            !nbDigits :: Int = floor (tmp + 1)
         in even nbDigits

applyRule :: (Int, Rule) -> [Int]
applyRule (_, R1) = [1]
applyRule (x, R3) = [x * 2024]
applyRule (x, R2) = r3 x
  where
    toString :: Int -> [Char]
    toString = show
    toInt :: [Char] -> Int
    toInt = read
    splitInTwo digits =
        let !halfLen = length digits `div` 2
            !firstHalf = toInt $ take halfLen digits
            !lastHalf = toInt $ drop halfLen digits
         in [firstHalf, lastHalf]
    r3 = splitInTwo . toString

part1 :: Int -> [Int] -> [Int]
part1 = loop
  where
    loop :: Int -> [Int] -> [Int]
    loop 0 term = term
    loop n term =
        loop (n - 1) . concatMap (applyRule . selectRule) $ term

-- TODO: Try basic memoization of part1 for part2

-- INFO: Thank you Cyril ! :
-- https://github.com/Alt-Net-AdventOfCode/AdventOfCode/blob/master/AdventCalendar2024/Day11/DupdobDay11.cs

part2 :: [Int] -> Int
part2 stones =
    let blinkThemAll = blink75 <$> stones
        sumByStones = HM.foldr' (+) 0 <$> blinkThemAll
     in sum sumByStones
  where
    blink75 :: Int -> HM.HashMap Int Int
    blink75 stone =
        runLoop (HM.singleton stone 1) 75

    runLoop :: HM.HashMap Int Int -> Int -> HM.HashMap Int Int
    runLoop mp' n
        | n == 0 = mp'
        | otherwise =
            let !currentStones = HM.toList mp'
                !nextMp = foldr (flip computeNextStones) HM.empty currentStones
             in runLoop nextMp (n - 1)

    computeNextStones :: HM.HashMap Int Int -> (Int, Int) -> HM.HashMap Int Int
    computeNextStones mp (int, quantity) =
        let !results = applyRule . selectRule $ int
         in keepCaching mp results
      where
        keepCaching mp' [] = mp'
        keepCaching mp' (int' : ints) =
            case HM.lookup int' mp' of
                Just x ->
                    let nm = HM.insert int' (x + quantity) mp'
                     in keepCaching nm ints
                Nothing ->
                    let nm = HM.insert int' quantity mp'
                     in keepCaching nm ints

day11 :: T.Text -> (Int, Int)
day11 input =
    let !ints = parse input
     in (length $ part1 25 ints, part2 ints)

parse :: T.Text -> [Int]
parse input =
    map (read . T.unpack) . T.splitOn (T.pack " ") $ T.strip input

-- INFO:attempt at optimization to speed up computation.

-- inefficientPart2 :: Int -> [Int] -> V.Vector Int
-- inefficientPart2 nTimes input = loop HM.empty nTimes $ V.fromList input
--   where
--     loop !cache !n term
--         | n == 0 = D.trace "WTF?" term
--         | otherwise =
--             let !updatedCache = D.trace ("number -> " ++ show n ++ "\n") cacheIt cache term
--                 !newTerms = V.concatMap (updatedCache HM.!) term
--                 !execute = loop updatedCache (n - 1) newTerms
--              in execute
--
--     cacheIt :: HM.HashMap Int (V.Vector Int) -> V.Vector Int -> HM.HashMap Int (V.Vector Int)
--     cacheIt !mp !terms
--         | V.length terms == 0 = D.trace "A cycle !" mp
--         | otherwise =
--             let !int = V.head terms
--                 !ints = V.tail terms
--              in ( if HM.member int mp
--                     then cacheIt mp ints
--                     else
--                         ( let !r = applyRule' . selectRule $ int
--                               !newMp = HM.insert int r mp
--                            in cacheIt newMp ints
--                         )
--                 )
--     -- INFO: Wanted to try an implementation with strict data structure, somehow faster but still crap
--     applyRule' :: (Int, Rule) -> V.Vector Int
--     applyRule' (_, R1) = V.singleton 1
--     applyRule' (x, R3) = V.singleton (x * 2024)
--     applyRule' (x, R2) = r3 x
--       where
--         toString :: Int -> [Char]
--         toString = show
--         toInt :: [Char] -> Int
--         toInt = read
--         splitInTwo digits =
--             let !halfLen = length digits `div` 2
--                 !firstHalf = toInt $ take halfLen digits
--                 !lastHalf = toInt $ drop halfLen digits
--              in V.fromList [firstHalf, lastHalf]
--         r3 = splitInTwo . toString
