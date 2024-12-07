module AOCDay6 (day6) where

import Data.Map qualified as Mp
import Data.Matrix qualified as Mx
import Data.Text qualified as T
import Data.Vector qualified as V

data GuardDirection = GFacingUp | GFacingDown | GFacingRight | GFacingLeft
    deriving (Eq)
type Position = (Int, Int)
data Guard = P GuardDirection Position
data CellInfo = Wall | Free | Outside
    deriving (Eq)

playerAvatarToDirection :: Mp.Map Char GuardDirection
playerAvatarToDirection =
    Mp.fromList $ zip ['^', '>', '<', 'v'] [GFacingUp, GFacingRight, GFacingLeft, GFacingDown]

changeDirection :: GuardDirection -> GuardDirection
changeDirection GFacingUp = GFacingRight
changeDirection GFacingDown = GFacingLeft
changeDirection GFacingRight = GFacingDown
changeDirection GFacingLeft = GFacingUp

computeNextPosition :: Guard -> Position
computeNextPosition (P direction (row, col))
    | direction == GFacingRight = (row, col + 1)
    | direction == GFacingLeft = (row, col - 1)
    | direction == GFacingUp = (row - 1, col)
    | direction == GFacingDown = (row + 1, col)
    | otherwise = error "WTF?"

move :: Guard -> Position -> Guard
move (P direction _) = P direction

getContent :: Mx.Matrix Char -> Position -> CellInfo
getContent mx pos =
    case uncurry Mx.safeGet pos mx of
        Nothing -> Outside
        Just '#' -> Wall
        _ -> Free

findGuard :: Mx.Matrix Char -> Guard
findGuard mx =
    search 1
  where
    search :: Int -> Guard
    search rowNb =
        case Mx.safeGetRow rowNb mx of
            Nothing -> error ("We did not find the player. row index is " ++ show rowNb)
            Just row ->
                case V.findIndex (`Mp.member` playerAvatarToDirection) row of
                    Nothing -> search (rowNb + 1)
                    Just avatarColIfx ->
                        let avatar = row V.! avatarColIfx
                            direction = playerAvatarToDirection Mp.! avatar
                            position = (rowNb, avatarColIfx + 1)
                         in P direction position

patrol :: Mx.Matrix Char -> [(Int, Int)] -> Guard -> [(Int, Int)]
patrol mx xs p@(P direction currentPos) =
    let nextPos = computeNextPosition p
        nextPosInfo = getContent mx nextPos
     in case nextPosInfo of
            Outside -> currentPos : xs
            Wall -> patrol mx xs (P (changeDirection direction) currentPos)
            Free ->
                if currentPos `elem` xs
                    then patrol mx xs (move p nextPos)
                    else patrol mx (currentPos : xs) (move p nextPos)

part1 :: Mx.Matrix Char -> Int
part1 mx =
    let player = findGuard mx
     in length $ patrol mx [] player

optimizedPart2_1 :: Mx.Matrix Char -> Int
optimizedPart2_1 mx =
    length . filter id $ startSimulation guardPatrolPositions
  where
    guard = findGuard mx
    guardPatrolPositions = patrol mx [] guard
    startSimulation :: [(Int, Int)] -> [Bool]
    startSimulation [] = []
    startSimulation ((row, col) : xs) =
        let version = Mx.setElem '#' (row, col) mx
         in continueSimulation [] guard version : startSimulation xs
    continueSimulation xs p@(P direction currentPos) mx' =
        let nextPos = (computeNextPosition p, direction)
            nextPosInfo = getContent mx' (fst nextPos)
         in case nextPosInfo of
                Outside -> False
                Wall ->
                    (nextPos `elem` xs)
                        || continueSimulation (nextPos : xs) (P (changeDirection direction) currentPos) mx'
                Free -> continueSimulation xs (move p (fst nextPos)) mx'

day6 :: T.Text -> (Int, Int)
day6 input =
    (part1 $ parse input, optimizedPart2_1 $ parse input)

{-- INFO: Previous unoptimized version
part2 :: Mx.Matrix Char -> Int
part2 mx =
    length . filter id $ simulate (1, 1)
  where
    player = findGuard mx
    playerPos = case player of (P _ p) -> p
    simulate :: (Int, Int) -> [Bool]
    simulate (row, col)
        | (row, col) == playerPos = simulate (row, col + 1)
        | getContent mx (row, col) == Wall = simulate (row, col + 1)
        | row > Mx.nrows mx = []
        | col > Mx.ncols mx = simulate (row + 1, 1)
        | otherwise =
            let version = Mx.setElem '#' (row, col) mx
             in doIt [] player version : simulate (row, col + 1)
    doIt xs p@(P direction currentPos) mx' =
        let nextPos = (computeNextPosition p, direction)
            nextPosInfo = getContent mx' (fst nextPos)
         in case nextPosInfo of
                Outside -> False
                Wall ->
                    (nextPos `elem` xs)
                        || doIt (nextPos : xs) (P (changeDirection direction) currentPos) mx'
                Free -> doIt xs (move p (fst nextPos)) mx' --}

parse :: T.Text -> Mx.Matrix Char
parse = Mx.fromLists . map T.unpack . T.lines
