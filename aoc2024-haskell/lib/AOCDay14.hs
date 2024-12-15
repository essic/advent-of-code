module AOCDay14 (day14) where

import Data.Attoparsec.Text qualified as AT

-- import Data.Foldable (maximumBy)
import Data.Matrix qualified as M
import Data.Text qualified as T
import Data.Tuple (swap)

-- import Data.Vector.Strict qualified as V

-- import Debug.Trace qualified as D

type Column = Int
type Row = Int

newtype Position = Position (Column, Row)
    deriving (Show)

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

tick :: Int -> Area -> [Robot] -> (Area,[Robot])
tick time area robots =
    let updatedRobots = computeRobotPosition area time <$> robots
        updatedArea = initializeArea updatedRobots area
     in (updatedArea,updatedRobots)
     where
        computeRobotPosition :: Area -> Int -> Robot -> Robot
        computeRobotPosition (Area m) time r@(Robot{p = Position position, v = Velocity velocity}) =
            let col = (fst position + time * fst velocity) `mod` M.ncols m
                row = (snd position + time * snd velocity) `mod` M.nrows m
                col' = if col < 1 then col + M.ncols m else col
                row' = if row < 1 then row + M.nrows m else row
                newPosition = Position (col', row')
             in Robot{p = newPosition, v = Velocity velocity}


smarterPart1 :: Int -> Int -> Int  -> [Robot] -> Int
smarterPart1 nTimes height widith robots =
    let area = Area $ M.zero height widith
        (updatedArea,updatedRobots) = tick nTimes area robots
        !quadrants = getQuadrants updatedArea
     in product $ sum <$> quadrants

--INFO: Wrong but we'll see ..
-- mostConsecutiveInTheMiddle :: Area -> Int
-- mostConsecutiveInTheMiddle (Area m) =
--   let middleCol = M.ncols m `div` 2
--       middleRow = M.nrows m `div` 2
--    in (sum (M.getRow middleRow m) + sum (M.getCol middleCol m))


day14 :: Int -> Int -> T.Text -> (Int, Int)
day14 widith height input =
    let part1 = smarterPart1 100 height widith robots  -- product $ sum <$> (getQuadrants . fst $ play 1 (area, robots))
     in (part1, 0)
  where
    robots =
        case AT.parseOnly parseRobots input of
            Right r -> r
            Left msg -> error msg
    area = initializeArea robots $ Area $ M.zero height widith

play :: Int -> (Area, [Robot]) -> (Area, [Robot])
play nTimes (area, robots) =
    loop nTimes (area, robots)
  where
    loop 0 content = content
    loop n content =
        loop (n - 1) (uncurry run content [])
    run a [] xs = (a, xs)
    run a (robot : xs) acc =
        let (updatedArea, updatedRobot) = robotTurn a robot
         in run updatedArea xs (updatedRobot : acc)

robotTurn :: Area -> Robot -> (Area, Robot)
robotTurn (Area m) Robot{p = Position currentPosition, v = Velocity (horizontal, vertical)} =
    let newPosition = (newCol, newRow)
        updatedMap = moveRobot currentPosition newPosition
     in (Area updatedMap, Robot{p = Position newPosition, v = Velocity (horizontal, vertical)})
  where
    moveRobot :: (Column, Row) -> (Column, Row) -> M.Matrix Int
    moveRobot from to =
        let fromContent = (uncurry M.getElem (swap from) m - 1)
            toContent = (uncurry M.getElem (swap to) m + 1)
            updatedM = M.setElem toContent (swap to) (M.setElem fromContent (swap from) m)
         in updatedM
    newCol =
        let deltaCol = fst currentPosition + horizontal
         in if deltaCol >= 1 && deltaCol <= M.ncols m
                then deltaCol
                else if deltaCol < 1 then M.ncols m + deltaCol else deltaCol - M.ncols m
    newRow =
        let deltaRow = snd currentPosition + vertical
         in if deltaRow >= 1 && deltaRow <= M.nrows m
                then deltaRow
                else if deltaRow < 1 then M.nrows m + deltaRow else deltaRow - M.nrows m


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
