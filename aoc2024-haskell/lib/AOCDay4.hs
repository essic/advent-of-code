{-# LANGUAGE DerivingVia #-}

module AOCDay4 (day4) where

import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Vector.Strict qualified as V

newtype SearchWindow a = SearchWindow [a]
    deriving (Show)

parse :: T.Text -> V.Vector (V.Vector Char)
parse =
    V.fromList . map (V.fromList . T.unpack) . T.lines

getRowLength :: V.Vector a -> Int
getRowLength = V.length

getColumnLength :: V.Vector (V.Vector a) -> Int
getColumnLength grid =
    case V.length <$> grid V.!? 0 of
        Just l -> l
        _ -> 0

getSearchWindows :: (Int, Int) -> V.Vector (V.Vector a) -> Int -> [SearchWindow a]
getSearchWindows (rIdx, cIdx) grid lengthWindow =
    catMaybes
        [ searchRight
        , searchLeft
        , searchUp
        , searchDown
        , searchDiagUpLeft
        , searchDiagDownLeft
        , searchDiagUpRight
        , searchDiagDownRight
        ]
  where
    searchDiagUpLeft =
        if cIdx + 1 - lengthWindow < 0 || rIdx - lengthWindow + 1 < 0
            then Nothing
            else
                let cIdxes = (+) cIdx <$> reverse [(-lengthWindow + 1) .. 0]
                    rIdxes = (+) rIdx <$> reverse [(-lengthWindow + 1) .. 0]
                    cells = (\(r, c) -> (grid V.! r) V.! c) <$> zip rIdxes cIdxes
                 in Just (SearchWindow cells)
    searchDiagDownLeft =
        if cIdx + 1 - lengthWindow < 0 || rIdx + lengthWindow > getRowLength grid
            then Nothing
            else
                let cIdxes = (+) cIdx <$> reverse [(-lengthWindow + 1) .. 0]
                    rIdxes = (+) rIdx <$> [0 .. lengthWindow - 1]
                    cells = (\(r, c) -> (grid V.! r) V.! c) <$> zip rIdxes cIdxes
                 in Just (SearchWindow cells)
    searchDiagUpRight =
        if cIdx + lengthWindow > getColumnLength grid || rIdx - lengthWindow + 1 < 0
            then Nothing
            else
                let cIdxes = (+) cIdx <$> [0 .. lengthWindow - 1]
                    rIdxes = (+) rIdx <$> reverse [(-lengthWindow + 1) .. 0]
                    cells = (\(r, c) -> (grid V.! r) V.! c) <$> zip rIdxes cIdxes
                 in Just (SearchWindow cells)
    searchDiagDownRight =
        if cIdx + lengthWindow > getColumnLength grid || rIdx + lengthWindow > getRowLength grid
            then Nothing
            else
                let cIdxes = (+) cIdx <$> [0 .. lengthWindow - 1]
                    rIdxes = (+) rIdx <$> [0 .. lengthWindow - 1]
                    cells = (\(r, c) -> (grid V.! r) V.! c) <$> zip rIdxes cIdxes
                 in Just (SearchWindow cells)
    searchRight =
        if cIdx + lengthWindow > getColumnLength grid
            then Nothing
            else Just . SearchWindow . V.toList . V.slice cIdx lengthWindow $ grid V.! rIdx

    searchLeft =
        if cIdx + 1 - lengthWindow < 0
            then Nothing
            else
                Just . SearchWindow . reverse . V.toList . V.slice (cIdx + 1 - lengthWindow) lengthWindow $ grid V.! rIdx

    searchUp =
        if rIdx - lengthWindow + 1 < 0
            then Nothing
            else
                Just . SearchWindow $
                    ((flip (V.!) cIdx . (V.!) grid) . (+) rIdx <$> reverse [(-lengthWindow + 1) .. 0])
    searchDown =
        if rIdx + lengthWindow > getRowLength grid
            then Nothing
            else
                Just . SearchWindow $
                    flip (V.!) cIdx . (V.!) grid <$> [rIdx .. (rIdx + lengthWindow - 1)]

part1 :: T.Text -> Int
part1 input =
    loop 0 0
  where
    grid = parse input
    maxRowLength = getRowLength grid
    maxColumnLength = getColumnLength grid
    xmasMatch (SearchWindow data2) =
        case data2 of
            "XMAS" -> 1
            _ -> 0
    loop :: Int -> Int -> Int
    loop rIdx cIdx
        | rIdx >= maxRowLength = 0
        | cIdx >= maxColumnLength = loop (rIdx + 1) 0
        | otherwise =
            let r = getSearchWindows (rIdx, cIdx) grid 4
             in sum (xmasMatch <$> r) + loop rIdx (cIdx + 1)

newtype SearchCrossWindow a = SearchCrossWindow ([a], [a])
    deriving (Show)

getCross :: (Int, Int) -> V.Vector (V.Vector a) -> Maybe (SearchCrossWindow a)
getCross (rIdx, cIdx) grid =
    let isRowIndexValid
            | rIdx - 1 >= 0 && rIdx + 1 < getRowLength grid = True
            | otherwise = False
        isColIndexValid
            | cIdx - 1 >= 0 && cIdx + 1 < getColumnLength grid = True
            | otherwise = False
     in ( if isRowIndexValid && isColIndexValid
            then
                Just $
                    SearchCrossWindow
                        (
                            [ (grid V.! (rIdx - 1)) V.! (cIdx - 1)
                            , (grid V.! rIdx) V.! cIdx
                            , (grid V.! (rIdx + 1)) V.! (cIdx + 1)
                            ]
                        ,
                            [ (grid V.! (rIdx + 1)) V.! (cIdx - 1)
                            , (grid V.! rIdx) V.! cIdx
                            , (grid V.! (rIdx - 1)) V.! (cIdx + 1)
                            ]
                        )
            else Nothing
        )

part2 :: T.Text -> Int
part2 input =
    loop 0 0
  where
    d = parse input
    maxRowLength = getRowLength d
    maxColumnLength = getColumnLength d
    xmasMatch (SearchCrossWindow i) =
        case i of
            ("MAS", "MAS") -> 1
            ("SAM", "SAM") -> 1
            ("MAS", "SAM") -> 1
            ("SAM", "MAS") -> 1
            _ -> 0
    loop :: Int -> Int -> Int
    loop rIdx cIdx
        | rIdx >= maxRowLength = 0
        | cIdx >= maxColumnLength = loop (rIdx + 1) 0
        | otherwise =
            let r = getCross (rIdx, cIdx) d
             in case r of
                    Nothing -> loop rIdx (cIdx + 1)
                    Just x -> xmasMatch x + loop rIdx (cIdx + 1)

day4 :: T.Text -> (Int, Int)
day4 input =
    (part1 input, part2 input)
