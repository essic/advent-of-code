module AOCDay10 (day10) where

import Data.Char (digitToInt)
import Data.List.Unique (occurrences)
import Data.Matrix qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V

day10 :: T.Text -> (Int, Int)
day10 input =
    part1 $ parse input

part1 :: M.Matrix Int -> (Int, Int)
part1 mx =
    (doItPart1 1, doItPart2 1)
  where
    up (row, col) = (row - 1, col)
    down (row, col) = (row + 1, col)
    left (row, col) = (row, col - 1)
    right (row, col) = (row, col + 1)
    next (r, c) = [up (r, c), down (r, c), left (r, c), right (r, c)]
    doItPart1 :: Int -> Int
    doItPart1 row =
        case map (+ 1) . V.toList . V.findIndices (== 0) <$> M.safeGetRow row mx of
            Nothing -> 0
            Just zeroes ->
                let r1 = concatMap (map length . concatMap snd . occurrences . walkPart1 0 row) zeroes
                 in sum r1 + doItPart1 (row + 1)
    doItPart2 :: Int -> Int
    doItPart2 row =
        case map (+ 1) . V.toList . V.findIndices (== 0) <$> M.safeGetRow row mx of
            Nothing -> 0
            Just zeroes ->
                let r2 = walkPart2 0 row <$> zeroes
                 in sum r2 + doItPart2 (row + 1)
    walkPart2 :: Int -> Int -> Int -> Int
    walkPart2 9 row col =
        if M.safeGet row col mx == Just 9
            then 1
            else 0
    walkPart2 value row col =
        let isValueFound = M.safeGet row col mx == Just value
            nextValue = value + 1
         in if isValueFound
                then sum (uncurry (walkPart2 nextValue) <$> next (row, col))
                else 0
    walkPart1 :: Int -> Int -> Int -> [(Int, Int)]
    walkPart1 9 row col =
        [(row, col) | M.safeGet row col mx == Just 9]
    walkPart1 value row col =
        let isValueFound = M.safeGet row col mx == Just value
            nextValue = value + 1
         in if isValueFound
                then concatMap (uncurry (walkPart1 nextValue)) (next (row, col))
                else []

parse :: T.Text -> M.Matrix Int
parse =
    M.fromLists . map (map digitToInt . T.unpack) . T.lines
