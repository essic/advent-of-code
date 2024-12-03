module AOCDay2 (day2) where

import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Vector.Strict qualified as V

data LevelState = Increasing | Decreasing | Neither | Default
    deriving (Eq)
data ReportState = Safe | Unsafe
    deriving (Eq)

computeLevelState :: Int -> Int -> LevelState
computeLevelState a b
    | a > b && (a - b) >= 1 && (a - b) <= 3 = Increasing
    | a < b && (b - a) >= 1 && (b - a) <= 3 = Decreasing
    | otherwise = Neither

computeNextReportState :: LevelState -> LevelState -> ReportState
computeNextReportState Increasing Increasing = Safe
computeNextReportState Decreasing Decreasing = Safe
computeNextReportState Default Increasing = Safe
computeNextReportState Default Decreasing = Safe
computeNextReportState _ _ = Unsafe

parse :: T.Text -> [V.Vector Int]
parse input =
    reports
  where
    separator = T.pack " "
    toInt x =
        case TR.decimal x of
            Right (n, _) -> (n :: Int)
            Left msg -> error msg
    reports = V.fromList . map toInt . T.splitOn separator <$> T.lines input

iteratePart1 :: V.Vector Int -> ReportState
iteratePart1 vect =
    sliding 0 Default
  where
    window offset windowSize =
        let isSizeValid = V.length vect >= offset + windowSize
            endPos = offset + windowSize - 1
         in (if isSizeValid then (V.!) vect <$> [offset .. endPos] else [])

    sliding :: Int -> LevelState -> ReportState
    sliding offset state =
        let win = window offset 2
         in case win of
                [] -> Safe
                [a, b] ->
                    let levelState = computeLevelState a b
                        nextReportState = computeNextReportState state levelState
                     in case nextReportState of
                            Unsafe -> Unsafe
                            Safe ->
                                sliding (offset + 1) levelState
                _ -> error "We should never reach here !"

iteratePart2 :: V.Vector Int -> ReportState
iteratePart2 vect =
    case iteratePart1 vect of
        Safe -> Safe
        _ -> runForRemainingVersions 0
  where
    runForRemainingVersions position
        | position == V.length vect =
            Unsafe
        | otherwise =
            let p1 = V.take position vect
                p2 = V.drop (position + 1) vect
             in case iteratePart1 (p1 <> p2) of
                    Safe -> Safe
                    _ -> runForRemainingVersions (position + 1)

part :: (V.Vector Int -> ReportState) -> [V.Vector Int] -> Int
part fn x =
    L.length . L.filter (== Safe) $ fn <$> x

day2 :: T.Text -> (Int, Int)
day2 input =
    (part iteratePart1 reports, part iteratePart2 reports)
  where
    reports = parse input
