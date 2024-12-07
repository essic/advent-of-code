module AOCDay7 (day7, cutSuffix) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Text.Read (readMaybe)

data Equation
    = Eqn
    { result :: Int
    , operands :: [Int]
    }
    deriving (Show)

part1 :: [Equation] -> Int
part1 eqns =
    sum $ part1' <$> eqns
  where
    part1' :: Equation -> Int
    part1' Eqn{result = r, operands = ints} =
        let stni = reverse ints
         in if compute r stni then r else 0
    compute :: Int -> [Int] -> Bool
    compute _ [] = False
    compute ir [a] =
        ((ir `div` a) == 1 && (ir `mod` a == 0)) || (ir - a) == 0
    compute ir (a : b : xs) =
        let c1 = ir `div` a
            modC1 = ir `mod` a
            c2 = ir - a
         in case (modC1 /= 0, c2 <= 0) of
                (True, True) -> False
                (False, False) -> compute c1 (b : xs) || compute c2 (b : xs)
                (True, False) -> compute c2 (b : xs)
                (False, True) -> compute c1 (b : xs)
cutSuffix :: String -> String -> Maybe (String, String)
cutSuffix toRemove from =
    let suffix = reverse . take (length toRemove) $ reverse from
        prefix = take (length from - length suffix) from
        verify = suffix == toRemove
     in if verify then Just (suffix, prefix) else Nothing

part2 :: [Equation] -> Int
part2 eqns =
    sum $ part2' <$> eqns
  where
    toInt :: String -> Maybe Int
    toInt = readMaybe
    part2' :: Equation -> Int
    part2' Eqn{result = r, operands = ints} =
        let stni = reverse ints
         in if compute r stni then r else 0
    compute :: Int -> [Int] -> Bool
    compute _ [] = False
    compute ir [a] =
        let irStr = show ir
            aStr = show a
            l = cutSuffix aStr irStr
            isValid =
                case l of
                    Just (_, "") -> True
                    _ -> False
         in ((ir `div` a) == 1 && (ir `mod` a == 0)) || (ir - a) == 0 || isValid
    compute ir (a : b : xs) =
        let c1 = ir `div` a
            modC1 = ir `mod` a
            c2 = ir - a
            aStr = show a
            irStr = show ir
            maybeSuffix = fromMaybe ("", "") (cutSuffix aStr irStr)
            c3 = fromMaybe 0 (toInt . snd $ maybeSuffix)
         in case (modC1 /= 0, c2 <= 0, maybeSuffix == ("", "")) of
                (True, True, True) -> False
                (False, False, False) -> compute c1 (b : xs) || compute c2 (b : xs) || compute c3 (b : xs)
                (False, True, True) -> compute c1 (b : xs)
                (True, False, True) -> compute c2 (b : xs)
                (True, True, False) -> compute c3 (b : xs)
                (False, False, True) -> compute c1 (b : xs) || compute c2 (b : xs)
                (False, True, False) -> compute c1 (b : xs) || compute c3 (b : xs)
                (True, False, False) -> compute c2 (b : xs) || compute c3 (b : xs)

day7 :: T.Text -> (Int, Int)
day7 input =
    let values = parse input
     in (part1 values, part2 values)

parse :: T.Text -> [Equation]
parse input = toEquation <$> T.lines input
  where
    toInt :: T.Text -> Int
    toInt = read . T.unpack
    toEquation :: T.Text -> Equation
    toEquation i =
        let components = T.splitOn (T.pack ":") i
            rawResult = head components
            rawOperands1 = T.strip $ T.concat (tail components)
            rawOperands2 = T.splitOn (T.pack " ") rawOperands1
         in Eqn{result = toInt rawResult, operands = toInt <$> rawOperands2}

-- INFO: old inefficient solution

data Op = Mul Int Int | Add Int Int | Concat Int Int
opToValue :: Op -> Int
opToValue (Mul a b) = a * b
opToValue (Add a b) = a + b
opToValue x@(Concat _ _) = read $ show x

instance Show Op where
    show (Mul a b) = show a ++ " * " ++ show b
    show (Add a b) = show a ++ " + " ++ show b
    show (Concat a b) = show a ++ show b

data TreePart1 = EmptyNode | Choice Op Op TreePart1 TreePart1
    deriving (Show)

newtype EquationStatus = Solved Int
    deriving (Show)

generateCasesPart1 :: [Int] -> TreePart1
generateCasesPart1 [] = EmptyNode
generateCasesPart1 [_] = EmptyNode
generateCasesPart1 (a : b : xs) =
    let c1 = Add a b
        c2 = Mul a b
     in Choice c1 c2 (generateCasesPart1 (opToValue c1 : xs)) (generateCasesPart1 (opToValue c2 : xs))

getNumberOfSolvedPart1 :: Equation -> (Equation, EquationStatus)
getNumberOfSolvedPart1 eqn@Eqn{result = r, operands = p} =
    let ps = generateCasesPart1 p
        results = findResults ps
        solvedNTimes = sum $ map (\x -> if x == r then 1 else 0) results
     in (eqn, Solved solvedNTimes)
  where
    findResults :: TreePart1 -> [Int]
    findResults EmptyNode = []
    findResults (Choice r1 r2 EmptyNode EmptyNode) = [opToValue r1, opToValue r2]
    findResults (Choice c1 c2 a1 a2) =
        let continueC1 = opToValue c1 < r
            continueC2 = opToValue c2 < r
         in case (continueC1, continueC2) of
                (True, True) -> findResults a1 ++ findResults a2
                (True, False) -> findResults a1
                (False, True) -> findResults a2
                (False, False) -> []

data TreePart2 = EmptyNode2 | Choice2 Op Op Op TreePart2 TreePart2 TreePart2
    deriving (Show)

generateCasesPart2 :: [Int] -> TreePart2
generateCasesPart2 [] = EmptyNode2
generateCasesPart2 [_] = EmptyNode2
generateCasesPart2 (a : b : xs) =
    let c1 = Add a b
        c2 = Mul a b
        c3 = Concat a b
     in Choice2
            c1
            c2
            c3
            (generateCasesPart2 (opToValue c1 : xs))
            (generateCasesPart2 (opToValue c2 : xs))
            (generateCasesPart2 (opToValue c3 : xs))

getNumberOfSolvedPart2 :: Equation -> (Equation, EquationStatus)
getNumberOfSolvedPart2 eqn@Eqn{result = r, operands = p} =
    let ps = generateCasesPart2 p
        results = findResults2 ps
        solvedNTimes = sum $ map (\x -> if x == r then 1 else 0) results
     in (eqn, Solved solvedNTimes)
  where
    findResults2 :: TreePart2 -> [Int]
    findResults2 EmptyNode2 = []
    findResults2 (Choice2 r1 r2 r3 EmptyNode2 EmptyNode2 EmptyNode2) =
        [opToValue r1, opToValue r2, opToValue r3]
    findResults2 (Choice2 c1 c2 c3 a1 a2 a3) =
        -- findResults2 a1 ++ findResults2 a2 ++ findResults2 a3
        let continueC1 = opToValue c1 < r
            continueC2 = opToValue c2 < r
            continueC3 = opToValue c3 < r
         in case (continueC1, continueC2, continueC3) of
                (True, True, True) -> findResults2 a1 ++ findResults2 a2 ++ findResults2 a3
                (True, False, False) -> findResults2 a1
                (True, True, False) -> findResults2 a1 ++ findResults2 a2
                (False, True, False) -> findResults2 a2
                (False, True, True) -> findResults2 a2 ++ findResults2 a3
                (False, False, True) -> findResults2 a3
                (True, False, True) -> findResults2 a1 ++ findResults2 a3
                (False, False, False) -> []
