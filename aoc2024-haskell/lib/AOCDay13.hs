module AOCDay13 (day13) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified as AT
import Data.Maybe (mapMaybe)
import Data.Text qualified as T

data Button = Button {bX :: Int, bY :: Int}
    deriving (Show)

data Prize = Prize {pX :: Int, pY :: Int}
    deriving (Show)

data Game = Game
    { a :: Button
    , b :: Button
    , prize :: Prize
    }
    deriving (Show)

solveGame :: Game -> Maybe (Int, Int)
solveGame Game{a = buttonA, b = buttonB, prize = prize} = do
    sa <- solForButtonA
    sb <- solForButtonB
    return (sa, sb)
  where
    nA = pX prize * bY buttonB - pY prize * bX buttonB
    dnA = bX buttonA * bY buttonB - bY buttonA * bX buttonB

    nB = pX prize * bY buttonA - pY prize * bX buttonA
    dnB = bX buttonB * bY buttonA - bY buttonB * bX buttonA

    solForButtonA =
        if dnA == 0 || (nA `mod` dnA) /= 0
            then Nothing
            else Just $ nA `div` dnA
    solForButtonB =
        if dnB == 0 || (nB `mod` dnB) /= 0
            then Nothing
            else Just $ nB `div` dnB

part1 :: [Game] -> Int
part1 games =
    sum . map applyScoring $ mapMaybe solveGame games
  where
    applyScoring (a, b) = if a > 100 || b > 100 then 0 else 3 * a + b

part2 :: [Game] -> Int
part2 games =
    sum . map applyScoring $ mapMaybe (solveGame . addToPrize) games
  where
    applyScoring (a, b) = 3 * a + b
    addToPrize g@Game{prize = prize} =
        Game
            { a = a g
            , b = b g
            , prize = Prize{pX = 10000000000000 + pX prize, pY = 10000000000000 + pY prize}
            }

day13 :: T.Text -> (Int, Int)
day13 input =
    case AT.parseOnly parseGames input of
        Right games -> (part1 games, part2 games)
        Left m -> error m

parseGames :: AT.Parser [Game]
parseGames =
    AT.many1 parserGame

parseButton :: AT.Parser Button
parseButton = do
    AT.string buttonLabel
    AT.string aLabel <|> AT.string bLabel
    AT.string xLabel
    x <- number
    AT.char ','
    AT.char ' '
    AT.string yLabel
    y <- number
    return Button{bX = x, bY = y}
  where
    buttonLabel = T.pack "Button "
    aLabel = T.pack "A: "
    bLabel = T.pack "B: "
    xLabel = T.pack "X+"
    yLabel = T.pack "Y+"
    number = round <$> AT.double

parsePrize :: AT.Parser Prize
parsePrize = do
    AT.string prizeLabel
    AT.string xLabel
    x <- number
    AT.char ','
    AT.char ' '
    AT.string yLabel
    y <- number
    return Prize{pX = x, pY = y}
  where
    prizeLabel = T.pack "Prize: "
    xLabel = T.pack "X="
    yLabel = T.pack "Y="
    number = round <$> AT.double

parserGame :: AT.Parser Game
parserGame = do
    AT.option '\n' (AT.char '\n')
    b1 <- parseButton
    AT.char '\n'
    b2 <- parseButton
    AT.char '\n'
    prize <- parsePrize
    AT.option '\n' (AT.char '\n')
    return Game{a = b1, b = b2, prize = prize}
