module AOCDay14 (day14) where

import Data.Attoparsec.Text qualified as AT

import Data.Foldable (maximumBy)
import Data.List.Unique (unique)
import Data.Matrix qualified as M
import Data.Text qualified as T
import Data.Tuple (swap)

-- import Data.Vector.Strict qualified as V

import Data.List (groupBy)
import Debug.Trace qualified as D

type Column = Int
type Row = Int

newtype Position = Position (Column, Row)
    deriving (Show, Eq)

newtype Velocity = Velocity (Int, Int)
    deriving (Show)

data Robot = Robot {p :: Position, v :: Velocity}
    deriving (Show)

newtype Area = Area (M.Matrix Int)
    deriving (Show)

getQuadrants :: Area -> [M.Matrix Int]
getQuadrants (Area m) =
    let middleCol = (M.ncols m + 1) `div` 2
        middleRow = (M.nrows m + 1) `div` 2
     in [ -- upper left
          M.submatrix 1 (middleRow - 1) 1 (middleCol - 1) m
        , -- upper right
          M.submatrix 1 (middleRow - 1) (middleCol + 1) (M.ncols m) m
        , -- lower left
          M.submatrix (middleRow + 1) (M.nrows m) 1 (middleCol - 1) m
        , -- lower right
          M.submatrix (middleRow + 1) (M.nrows m) (middleCol + 1) (M.ncols m) m
        ]

initializeArea :: [Robot] -> Area -> Area
initializeArea robots area = foldr setRobot area robots
  where
    setRobot :: Robot -> Area -> Area
    setRobot Robot{p = Position position} (Area m) =
        let currentValue = uncurry M.getElem (swap position) m
         in Area $ M.setElem (currentValue + 1) (swap position) m

tick :: Int -> Area -> [Robot] -> (Area, [Robot])
tick frame area robots =
    let updatedRobots = computeRobotPosition area <$> robots
        updatedArea = initializeArea updatedRobots area
     in (updatedArea, updatedRobots)
  where
    computeRobotPosition (Area m) (Robot{p = Position position, v = Velocity velocity}) =
        let col = (fst position + frame * fst velocity) `mod` M.ncols m
            row = (snd position + frame * snd velocity) `mod` M.nrows m
            col' = if col < 1 then col + M.ncols m else col
            row' = if row < 1 then row + M.nrows m else row
            newPosition = Position (col', row')
         in Robot{p = newPosition, v = Velocity velocity}

-- WARN: It does not work T_T
part2 :: Int -> Int -> Int -> [Robot] -> Int
part2 lastFrame height widith robots =
    let r = loop 1 []
        result = D.trace (show $ length r) $ maximumBy (\(nb1, _) (nb2, _) -> compare nb1 nb2) r
        (uArea, uRobots) = tick (snd result) (Area $ M.zero height widith) robots
        toRender = initializeArea uRobots uArea
     in D.trace (show toRender) $ snd result
  where
    area = Area $ M.zero height widith
    middlePosition =
        let (Area m) = area
         in ((M.ncols m + 1) `div` 2, (M.nrows m + 1) `div` 2)
    toPosition (Robot{p = Position position}) = position
    robotInTheMiddle (Robot{p = Position position}) = fst position == fst middlePosition
    loop frame results
        | frame > lastFrame = results
        | otherwise =
            let (_, updatedRobots) = tick frame area robots
                toto = groupBy (\p1 p2 -> fst p1 == fst p2) (toPosition <$> updatedRobots)
                toto1 = groupBy (\p1 p2 -> snd p1 == snd p2) (toPosition <$> updatedRobots)
                tata = maximum $ length <$> toto
                tata1 = maximum $ length <$> toto1
                nbRobotsInTheMiddle = length . unique $ map (snd . toPosition) (filter robotInTheMiddle updatedRobots)
             in loop (frame + 1) ((nbRobotsInTheMiddle, frame) : results)

smarterPart1 :: Int -> Int -> Int -> [Robot] -> Int
smarterPart1 nTimes height widith robots =
    let area = Area $ M.zero height widith
        (updatedArea, _) = tick nTimes area robots
        !quadrants = getQuadrants updatedArea
     in product $ sum <$> quadrants

-- INFO: Wrong but we'll see ..
-- mostConsecutiveInTheMiddle :: Area -> Int
-- mostConsecutiveInTheMiddle (Area m) =
--   let middleCol = M.ncols m `div` 2
--       middleRow = M.nrows m `div` 2
--    in (sum (M.getRow middleRow m) + sum (M.getCol middleCol m))

day14 :: Int -> Int -> T.Text -> (Int, Int)
day14 widith height input =
    let part1 = smarterPart1 100 height widith robots
        r2 = part2 100000 height widith robots
     in (part1, r2)
  where
    robots =
        case AT.parseOnly parseRobots input of
            Right r -> r
            Left msg -> error msg

-- INFO: First implementation
-- play :: Int -> (Area, [Robot]) -> (Area, [Robot])
-- play nTimes (area, robots) =
--     loop nTimes (area, robots)
--   where
--     loop 0 content = content
--     loop n content =
--         loop (n - 1) (uncurry run content [])
--     run a [] xs = (a, xs)
--     run a (robot : xs) acc =
--         let (updatedArea, updatedRobot) = robotTurn a robot
--          in run updatedArea xs (updatedRobot : acc)
-- robotTurn :: Area -> Robot -> (Area, Robot)
-- robotTurn (Area m) Robot{p = Position currentPosition, v = Velocity (horizontal, vertical)} =
--     let newPosition = (newCol, newRow)
--         updatedMap = moveRobot currentPosition newPosition
--      in (Area updatedMap, Robot{p = Position newPosition, v = Velocity (horizontal, vertical)})
--   where
--     moveRobot :: (Column, Row) -> (Column, Row) -> M.Matrix Int
--     moveRobot from to =
--         let fromContent = (uncurry M.getElem (swap from) m - 1)
--             toContent = (uncurry M.getElem (swap to) m + 1)
--             updatedM = M.setElem toContent (swap to) (M.setElem fromContent (swap from) m)
--          in updatedM
--     newCol =
--         let deltaCol = fst currentPosition + horizontal
--          in if deltaCol >= 1 && deltaCol <= M.ncols m
--                 then deltaCol
--                 else if deltaCol < 1 then M.ncols m + deltaCol else deltaCol - M.ncols m
--     newRow =
--         let deltaRow = snd currentPosition + vertical
--          in if deltaRow >= 1 && deltaRow <= M.nrows m
--                 then deltaRow
--                 else if deltaRow < 1 then M.nrows m + deltaRow else deltaRow - M.nrows m

parseRobots :: AT.Parser [Robot]
parseRobots =
    AT.many1 parseRobot
parseRobot :: AT.Parser Robot
parseRobot = do
    AT.option '\n' (AT.char '\n')
    position <- parsePosition
    AT.char ' '
    velocity <- parseVelocity
    return $ Robot{p = position, v = velocity}

parsePosition :: AT.Parser Position
parsePosition = do
    AT.string (T.pack "p=")
    row <- number
    AT.char ','
    col <- number
    return $ Position (row + 1, col + 1)
  where
    number = read <$> AT.many1 AT.digit

parseVelocity :: AT.Parser Velocity
parseVelocity = do
    AT.string (T.pack "v=")
    signHorizontal <- AT.option '+' (AT.char '-')
    horizontal <- number
    AT.char ','
    signVertical <- AT.option '+' (AT.char '-')
    vertical <- number
    AT.many' (AT.char ' ')
    let horizontal' = if signHorizontal == '-' then horizontal * (-1) else horizontal
    let vertical' = if signVertical == '-' then vertical * (-1) else vertical
    return $ Velocity (horizontal', vertical')
  where
    number = read <$> AT.many1 AT.digit
