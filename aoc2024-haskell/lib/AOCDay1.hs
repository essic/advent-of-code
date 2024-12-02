module AOCDay1 (day1) where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Read qualified as TR

sideBySide :: [[Int]] -> ([Int], [Int])
sideBySide input =
    doIt input [] []
  where
    doIt ((x : y : _) : zs) xs ys =
        doIt zs (x : xs) (y : ys)
    doIt _ xs ys = (xs, ys)

parseInput :: T.Text -> [[Int]]
parseInput x = numbers
  where
    separator = T.pack "   "
    rawLines = T.lines x
    rawNum =
        T.splitOn separator <$> rawLines
    numbers =
        map toInt <$> rawNum
    toInt t =
        case TR.decimal t of
            Right (n, _) -> (n :: Int)
            Left _ -> error "Error during parsing ! This should never happen though !"

day1Part1 :: T.Text -> Int
day1Part1 =
    sum . map compute <$> uncurry L.zip . sorting . sideBySide . parseInput
  where
    sorting (xs, ys) = (L.sort xs, L.sort ys)
    compute (a, b)
        | a >= b = a - b
        | otherwise = b - a

day2Part2 :: T.Text -> Int
day2Part2 x =
    L.foldl'
        ( \total n ->
            case M.lookup n sim of
                Nothing -> total
                Just occurence -> n * occurence + total
        )
        0
        left
  where
    (left, right) = sideBySide . parseInput $ x
    sim = mkSim right M.empty
    mkSim [] dict = dict
    mkSim (y : ys) dict
        | M.member y dict = mkSim ys (M.insertWith (+) y 1 dict)
        | otherwise = mkSim ys (M.insert y 1 dict)

day1 :: T.Text -> (Int, Int)
day1 x =
    (day1Part1 x, day2Part2 x)
