{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import AOCDay1 qualified as D1
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

main :: IO ()
main = hspec $ do
    describe "Day 1" $ do
        it "- part 1" $ do
            let (res, _) = D1.day1 inputDay1
             in res `shouldBe` 11
        it "- part 2" $ do
            let (_, res) = D1.day1 inputDay1
             in res `shouldBe` 31
