{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import AOCDay1
import AOCDay2
import AOCDay3
import AOCDay4
import AOCDay5
import AOCDay6
import Data.String.QQ
import Data.Text qualified as T
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Day 1" $ do
        it "- part 1" $ do
            let (res, _) = day1 inputDay1
             in res `shouldBe` 11
        it "- part 2" $ do
            let (_, res) = day1 inputDay1
             in res `shouldBe` 31
    describe "Day 2" $ do
        it "- part 1" $ do
            let (res, _) = day2 inputDay2
             in res `shouldBe` 2
        it "- part 2" $ do
            let (_, res) = day2 inputDay2
             in res `shouldBe` 4
    describe "Day 3" $ do
        it "- part 1" $ do
            let (res, _) = day3 inputDay3Part1
             in res `shouldBe` 161
        it "- part 2" $ do
            let (_, res) = day3 inputDay3Part2
             in res `shouldBe` 48
    describe "Day 4" $ do
        it "- part 1" $ do
            let (res, _) = day4 inputDay4
             in res `shouldBe` 18
        it "- part 2" $ do
            let (_, res) = day4 inputDay4
             in res `shouldBe` 9
    describe "Day 5" $ do
        it "- part 1" $ do
            let (res, _) = day5 inputDay5
             in res `shouldBe` 143
        it "- part 2" $ do
            let (_, res) = day5 inputDay5
             in res `shouldBe` 123
    describe "Day 6" $ do
        it "- part 1" $ do
            let (res, _) = day6 inputDay6
             in res `shouldBe` 41
        it "- part 2" $ do
            let (_, res) = day6 inputDay6
             in res `shouldBe` 6

inputDay1 :: T.Text
inputDay1 =
    [s|3   4
4   3
2   5
1   3
3   9
3   3|]

inputDay2 :: T.Text
inputDay2 =
    [s|7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9|]

inputDay3Part1 :: T.Text
inputDay3Part1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

inputDay3Part2 :: T.Text
inputDay3Part2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

inputDay4 :: T.Text
inputDay4 =
    [s|MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX|]

inputDay5 :: T.Text
inputDay5 =
    [s|47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47|]

inputDay6 :: T.Text
inputDay6 =
    [s|....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...|]
