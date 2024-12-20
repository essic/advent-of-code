module AOCDay8 (day8) where

import Data.Char (isDigit, isLetter)
import Data.List (tails)
import Data.List.Unique (occurrences)
import Data.Map.Strict qualified as Mp
import Data.Matrix qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V

type Row = Int
type Col = Int
data Antena = Antena {identifier :: Char, position :: (Row, Col)}
data Antinodes = Antinodes {for :: Char, first :: (Row, Col), second :: (Row, Col)}

groupAntenas :: M.Matrix Char -> Mp.Map Char [Antena]
groupAntenas grid =
    doGroup Mp.empty $ search [] 1
  where
    search antenas r =
        case M.safeGetRow r grid of
            Nothing -> antenas
            Just vect ->
                let b =
                        (\x -> Antena{identifier = M.getElem r (x + 1) grid, position = (r, x + 1)})
                            <$> V.findIndices (\a -> isDigit a || isLetter a) vect
                    bs = V.toList b
                 in search (bs ++ antenas) (r + 1)
    doGroup :: Mp.Map Char [Antena] -> [Antena] -> Mp.Map Char [Antena]
    doGroup antenasMap [] = antenasMap
    doGroup antenasMap (a@(Antena{identifier = c}) : antenas) =
        let updatedMap
                | Mp.member c antenasMap =
                    Mp.insert c (a : (antenasMap Mp.! c)) antenasMap
                | otherwise = Mp.insert c [a] antenasMap
         in doGroup updatedMap antenas

-- TODO: Improve overall design
part1 :: M.Matrix Char -> Int
part1 grid =
    let antenasByFrequency = groupAntenas grid
        antinodesByFrequencies = concatMap computeAllAntinodes $ Mp.elems antenasByFrequency
        result = countValidAntinodes antinodesByFrequencies
     in result
  where
    computeAllAntinodes :: [Antena] -> [Antinodes]
    computeAllAntinodes antenas =
        [ createAntinodes a b
        | (a : xs) <- tails antenas
        , b <- xs
        ]
    createAntinodes :: Antena -> Antena -> Antinodes
    createAntinodes first@(Antena{position = (r1, c1)}) second@(Antena{position = (r2, c2)}) =
        if identifier first /= identifier second
            then error "It should not happen here"
            else
                Antinodes
                    { for = identifier first
                    , first = (r1 - r2 + r1, c1 - c2 + c1)
                    , second = (r2 - r1 + r2, c2 - c1 + c2)
                    }

    countValidAntinodes :: [Antinodes] -> Int
    countValidAntinodes antinodes =
        let maxRow = M.nrows grid
            maxCol = M.ncols grid
            allRowCols = concatMap (\Antinodes{first = p1, second = p2} -> [p1, p2]) antinodes
            filtered =
                filter
                    ( \(row, col) ->
                        row >= 1
                            && row <= maxRow
                            && col >= 1
                            && col <= maxCol
                    )
                    allRowCols
            results = concatMap snd (occurrences filtered)
         in length results

-- INFO: Thanks to hopingforabetterpast on https://www.reddit.com/user/hopingforabetterpast/
part2 :: M.Matrix Char -> Int
part2 grid =
    sum antinodeGrid
  where
    antenas = Mp.elems $ groupAntenas grid
    isCellAntinode :: [(Row, Col) -> Bool]
    isCellAntinode =
        concatMap
            ( \xs ->
                [ \(x, y) -> (y - ya) * (xb - xa) == (yb - ya) * (x - xa)
                | ((Antena{position = (xa, ya)}) : as) <- tails xs
                , Antena{position = (xb, yb)} <- as
                ]
            )
            antenas

    antinodeGrid =
        M.mapPos
            ( \pos _ ->
                if any (\f -> f pos) isCellAntinode
                    then 1
                    else 0
            )
            grid

day8 :: T.Text -> (Int, Int)
day8 input =
    let area = parse input
     in (part1 area, part2 area)

parse :: T.Text -> M.Matrix Char
parse input =
    antenaMap
  where
    antenaMap = M.fromLists $ T.unpack <$> T.lines input
