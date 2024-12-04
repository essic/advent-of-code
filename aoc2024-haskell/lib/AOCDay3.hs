{-# LANGUAGE LambdaCase #-}

module AOCDay3 (day3) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified as AT
import Data.Char (isDigit)
import Data.Text qualified as T

newtype Mul = M (Int, Int)
    deriving (Show)

data Mul2
    = Op (Int, Int)
    | DoIt
    | DoNotDoIt
    deriving (Show)

parseValidNumber :: AT.Parser Int
parseValidNumber =
    toInt <$> (threeDigits <|> twoDigits <|> oneDigit)
  where
    toInt :: [Char] -> Int
    toInt = read

    parseDigit :: AT.Parser Char
    parseDigit = AT.satisfy isDigit

    threeDigits :: AT.Parser [Char]
    threeDigits = do
        d1 <- parseDigit
        d2 <- parseDigit
        d3 <- parseDigit
        return [d1, d2, d3]

    twoDigits :: AT.Parser [Char]
    twoDigits = do
        d1 <- parseDigit
        d2 <- parseDigit
        return [d1, d2]

    oneDigit :: AT.Parser [Char]
    oneDigit = do
        d1 <- parseDigit
        return [d1]

runParser :: AT.Parser a -> T.Text -> [a]
runParser p input =
    case AT.parse p input of
        (AT.Done nextInput r) ->
            if nextInput == T.pack ""
                then [r]
                else r : runParser p nextInput
        AT.Fail nextInput _ _ -> runParser p nextInput
        _ -> []

parserPart1 :: AT.Parser Mul
parserPart1 = do
    _ <- AT.manyTill AT.anyChar (AT.string $ T.pack "mul(")
    num1 <- parseValidNumber
    AT.char ','
    num2 <- parseValidNumber
    AT.char ')'
    return $ M (num1, num2)

part1 :: T.Text -> Int
part1 = sum . map (\(M x) -> uncurry (*) x) <$> runParser parserPart1

parserPart2 :: AT.Parser Mul2
parserPart2 =
    do
        _ <- AT.manyTill AT.anyChar (AT.char 'd' <|> AT.char 'm')
        T.unpack <$> (AT.string tokenDoNot <|> AT.string tokenDo <|> AT.string tokenMul)
        >>= ( \case
                "o()" -> return DoIt
                "on't()" -> return DoNotDoIt
                _ -> do
                    num1 <- parseValidNumber
                    AT.char ','
                    num2 <- parseValidNumber
                    AT.char ')'
                    return $ Op (num1, num2)
            )
  where
    tokenDo = T.pack "o()"
    tokenDoNot = T.pack "on't()"
    tokenMul = T.pack "ul("

-- INFO: non optimized version
-- part2 :: T.Text -> Int
-- part2 =
--     sum . map (uncurry (*)) . customFilter True . runParser parserPart2
--   where
--     customFilter :: Bool -> [Mul2] -> [(Int, Int)]
--     customFilter _ [] = []
--     customFilter True ((Op x) : xs') = x : customFilter True xs'
--     customFilter False (Op _ : xs') = customFilter False xs'
--     customFilter _ (DoNotDoIt : xs') = customFilter False xs'
--     customFilter _ (DoIt : xs') = customFilter True xs'

-- INFO: We avoid useless parsing and compute the operation during parsing
-- INFO: We always run until Partial result.
optimizedParserPart2 :: AT.Parser Mul2 -> T.Text -> Int
optimizedParserPart2 p input =
    case AT.parse p input of
        AT.Done nextInput r ->
            case r of
                (Op x) -> uncurry (*) x + optimizedParserPart2 parserPart2 nextInput
                DoIt -> optimizedParserPart2 parserPart2 nextInput
                DoNotDoIt -> optimizedParserPart2 parseUntilDo nextInput
        AT.Fail nextInput _ _ -> optimizedParserPart2 parserPart2 nextInput
        _ -> 0
  where
    parseUntilDo :: AT.Parser Mul2
    parseUntilDo = do
        _ <- AT.manyTill AT.anyChar (AT.string $ T.pack "do()")
        return DoIt

optimizedPart2 :: T.Text -> Int
optimizedPart2 = optimizedParserPart2 parserPart2

day3 :: T.Text -> (Int, Int)
day3 input =
    (part1 input, optimizedPart2 input)
