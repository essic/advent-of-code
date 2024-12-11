module AOCDay8 (day8) where

import Data.Char (isDigit, isLetter)
import Data.Matrix qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V

type Row = Int
type Col = Int
data Antena = A Char Row Col

findAntenas :: M.Matrix Char -> [Antena]
findAntenas mx =
    search [] 1
  where
    search antenas r =
        case M.safeGetRow r mx of
            Nothing -> antenas
            Just vect ->
                let b =
                        (\x -> A (M.getElem r (x + 1) mx) r (x + 1))
                            <$> V.findIndices (\a -> isDigit a || isLetter a) vect
                    bs = V.toList b
                 in search (bs ++ antenas) (r + 1)

day8 :: T.Text -> (Int, Int)
day8 input =
    (length . findAntenas $ parse input, 0)

parse :: T.Text -> M.Matrix Char
parse input =
    antenaMap
  where
    antenaMap = M.fromLists $ T.unpack <$> T.lines input
