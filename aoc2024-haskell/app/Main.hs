module Main where

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
import Control.Monad (when)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import System.Environment qualified as E
import System.IO qualified as IO

ctx :: M.Map [Char] (T.Text -> (Int, Int))
ctx =
    M.fromList
        [ ("data/day1.txt", day1)
        , ("data/day2.txt", day2)
        , ("data/day3.txt", day3)
        , ("data/day4.txt", day4)
        , ("data/day5.txt", day5)
        , ("data/day6.txt", day6)
        , ("data/day7.txt", day7)
        , ("data/day8.txt", day8)
        , ("data/day9.txt", day9)
        , ("data/day10.txt", day10)
        , ("data/day11.txt", day11)
        , -- day 12 missing
          ("data/day13.txt", day13)
        , ("data/day14.txt", day14 101 103)
        ]

data RunParam
    = All
    | SpecificDay String
    deriving (Eq)

main :: IO ()
main = do
    selection <- specificDay
    mapM_
        ( \(path, fn) ->
            when (selection == All || selection == SpecificDay path) $ do
                fh <- IO.openFile path IO.ReadMode
                content <- IO.hGetContents fh
                putStr $ path ++ " -> "
                print . fn . T.pack $ content
                IO.hClose fh
        )
        (M.toList ctx)
  where
    specificDay = do
        inputs <- E.getArgs
        return $
            case inputs of
                [] -> All
                [input] -> toFileName input
                _ -> error "We do not support multiple parameters !"
    toFileName input =
        case TR.decimal . T.pack $ input of
            Right (day :: Int, _) ->
                if day >= 1 && day <= 25
                    then
                        SpecificDay $ "data/day" ++ show day ++ ".txt"
                    else
                        error "Invalid day !"
            Left msg -> error msg
