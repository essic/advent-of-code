module Main where

import AOCDay1
import AOCDay2
import Control.Monad (when)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import System.Environment qualified as E
import System.IO qualified as IO

ctx :: M.Map [Char] (T.Text -> (Int, Int))
ctx =
    M.fromList [("data/day1.txt", day1), ("data/day2.txt", day2)]

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
