{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import AOCDay1 qualified as D1
import AOCDay2 qualified as D2
import AOCDay3 qualified as D3
import Data.String.QQ
import Data.Text qualified as T
import Test.Hspec

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

main :: IO ()
main = hspec $ do
    describe "Day 1" $ do
        it "- part 1" $ do
            let (res, _) = D1.day1 inputDay1
             in res `shouldBe` 11
        it "- part 2" $ do
            let (_, res) = D1.day1 inputDay1
             in res `shouldBe` 31
    describe "Day 2" $ do
        it "- part 1" $ do
            let (res, _) = D2.day2 inputDay2
             in res `shouldBe` 2
        it "- part 2" $ do
            let (_, res) = D2.day2 inputDay2
             in res `shouldBe` 4
    describe "Day 3" $ do
        it "- part 1" $ do
            let (res, _) = D3.day3 inputDay3Part1
             in res `shouldBe` 161
        it "- part 2" $ do
            let (_, res) = D3.day3 inputDay3Part2
             in res `shouldBe` 48
