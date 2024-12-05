module AOCDay5 (day5) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text as AT
import Data.Char (isDigit)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Text qualified as T

arePagesToProduceInEachUpdatesOrdered :: [M.Map Int Int] -> [Int] -> Bool
arePagesToProduceInEachUpdatesOrdered order =
    verify []
  where
    verify _ [] = True
    verify xs (y : ys) =
        let mustBeAfterY = concatMap (map snd . M.toList . M.filterWithKey (\k _ -> k == y)) order
            areElementsAllAfterY = foldl (\acc y' -> acc && notElem y' mustBeAfterY) True xs
         in areElementsAllAfterY && verify (y : xs) ys

findMiddleOfPagesToProduceInEachOrder :: [Int] -> Int
findMiddleOfPagesToProduceInEachOrder = doIt []
  where
    doIt _ [] = error "This should not happen!"
    doIt xs (y : ys)
        | L.length xs == L.length ys = y
        | L.length xs > L.length ys = error "This should not happen!"
        | otherwise = doIt (y : xs) ys

part1 :: T.Text -> Int
part1 input =
    sum $
        findMiddleOfPagesToProduceInEachOrder
            <$> L.filter (arePagesToProduceInEachUpdatesOrdered order) allUpdates
  where
    content = runParser input
    order = uncurry M.singleton <$> fst content
    allUpdates = snd content

sortPagesToProduceInEachUpdate :: [M.Map Int Int] -> [Int] -> [Int]
sortPagesToProduceInEachUpdate order xs =
    if arePagesToProduceInEachUpdatesOrdered order xs
        then xs
        else sortPagesToProduceInEachUpdate order (bubbleSort xs)
  where
    bubbleSort [] = []
    bubbleSort [x] = [x]
    bubbleSort (x : y : xs') =
        let mustBeAfter = concatMap (map snd . M.toList . M.filterWithKey (\k _ -> k == y)) order
            shouldSwap = L.elem x mustBeAfter
         in if shouldSwap then y : bubbleSort (x : xs') else x : bubbleSort (y : xs')

part2 :: T.Text -> Int
part2 input =
    sum $
        findMiddleOfPagesToProduceInEachOrder . sortPagesToProduceInEachUpdate order
            <$> L.filter (not . arePagesToProduceInEachUpdatesOrdered order) allUpdates
  where
    content = runParser input
    order = uncurry M.singleton <$> fst content
    allUpdates = snd content

day5 :: T.Text -> (Int, Int)
day5 input =
    (part1 input, part2 input)

toInt :: [Char] -> Int
toInt = read

parserForPageOrderingRules :: AT.Parser (Int, Int)
parserForPageOrderingRules = do
    a <- AT.many1 (AT.satisfy isDigit)
    AT.char '|'
    b <- AT.many1 (AT.satisfy isDigit)
    AT.char '\n'
    return (toInt a, toInt b)

parserForPagesToProduceInEachUpdates :: AT.Parser [Int]
parserForPagesToProduceInEachUpdates = do
    AT.char '\n'
    AT.many1 n
  where
    n :: AT.Parser Int
    n =
        toInt <$> (AT.manyTill (AT.satisfy isDigit) (AT.char ',') <|> AT.many1 (AT.satisfy isDigit))

runParser :: T.Text -> ([(Int, Int)], [[Int]])
runParser input =
    case AT.parseOnly p input of
        Right r -> r
        Left msg -> error msg
  where
    p = do
        rules <- AT.many1 parserForPageOrderingRules
        updates <- AT.many1 parserForPagesToProduceInEachUpdates
        return (rules, updates)
