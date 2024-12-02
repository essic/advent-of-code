{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import AOCDay1 qualified as D1
import AOCDay2 qualified as D2
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
