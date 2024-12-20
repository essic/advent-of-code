{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import AOCDay1
import AOCDay10
import AOCDay11
import AOCDay13
import AOCDay14
import AOCDay2
import AOCDay3
import AOCDay4
import AOCDay5
import AOCDay6
import AOCDay7
import AOCDay8
import AOCDay9
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
        it "- code_report version part 1" $ do
            let (res, _) = day3 inputDay3Part1
             in res `shouldBe` 161
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
    describe "Day 7" $ do
        it "- part 1" $ do
            let (res, _) = day7 inputDay7
             in res `shouldBe` 3749
        it "- part 2" $ do
            let (_, res) = day7 inputDay7
             in res `shouldBe` 11387
    describe "Day 8" $ do
        it "- part 1" $ do
            let (res, _) = day8 inputDay8
             in res `shouldBe` 14
        it "- part 2" $ do
            let (_, res) = day8 inputDay8
             in res `shouldBe` 34
    describe "Day 9" $ do
        it "- part 1" $ do
            let (res, _) = day9 inputDay9
             in res `shouldBe` 1928
        it "- part 2" $ do
            let (_, res) = day9 inputDay9
             in res `shouldBe` 2858
    describe "Day 10" $ do
        it "- part 1" $ do
            let (res, _) = day10 inputDay10
             in res `shouldBe` 36
        it "- part 2" $ do
            let (_, res) = day10 inputDay10
             in res `shouldBe` 81
    describe "Day 11" $ do
        it "- part 1" $ do
            let (res, _) = day11 inputDay11
             in res `shouldBe` 55312
    it "- part 2" $ do
        let (_, res) = day11 inputDay11
         in res `shouldBe` 65601038650482
    describe "Day 13" $ do
        it "- part 1" $ do
            let (res, _) = day13 inputDay13
             in res `shouldBe` 480
        it "- part 2" $ do
            let (_, res) = day13 inputDay13
             in res `shouldBe` 875318608908
    describe "Day 14" $ do
        it "- part 1" $ do
            let (res, _) = day14 11 7 inputDay14
             in res `shouldBe` 12
        it "- part 2" $ do
            let (_, res) = day14 11 7 inputDay14
             in res `shouldBe` 12

inputDay14 :: T.Text
inputDay14 =
    [s|p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3|]

inputDay13 :: T.Text
inputDay13 =
    [s|Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
|]

inputDay11 :: T.Text
inputDay11 = [s|125 17|]

inputDay10 :: T.Text
inputDay10 =
    [s|89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732|]

inputDay9 :: T.Text
inputDay9 =
    [s|2333133121414131402|]

inputDay8 :: T.Text
inputDay8 =
    [s|............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............|]

inputDay7 :: T.Text
inputDay7 =
    [s|190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20|]

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
