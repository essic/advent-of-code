module Main where

import AOCDay1
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import System.Environment qualified as E
import System.IO qualified as IO

ctx :: M.Map [Char] (T.Text -> (Int, Int))
ctx =
    M.fromList [("data/day1.txt", day1)]

main :: IO ()
main = do
    selection <- specificDay
    -- TODO: refactor in due time !
    case selection of
        Nothing ->
            mapM_
                ( \(path, fn) -> do
                    fh <- IO.openFile path IO.ReadMode
                    content <- IO.hGetContents fh
                    putStr $ path ++ " -> "
                    print . fn . T.pack $ content
                    IO.hClose fh
                )
                (M.toList ctx)
        Just fileName -> do
            fh <- IO.openFile fileName IO.ReadMode
            content <- IO.hGetContents fh
            putStr $ fileName ++ " -> "
            mapM_ (\fn -> print . fn . T.pack $ content) (M.lookup fileName ctx)
            IO.hClose fh
            return ()
  where
    specificDay = do
        inputs <- E.getArgs
        return $
            case inputs of
                [] -> Nothing
                [input] -> toFileName input
                _ -> error "We do not support multiple parameters !"
    toFileName input =
        case TR.decimal . T.pack $ input of
            Right (day :: Int, _) ->
                if day >= 1 && day <= 31
                    then
                        Just $ "data/day" ++ show day ++ ".txt"
                    else
                        error "Invalid day !"
            Left msg -> error msg
