{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- INFO: Several version exists, it would be interesting to check the difference of performance
-- Alternative versions at the end of the file, some basic measurement is also in the repo.
module AOCDay7 (day7) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Text.Read (readMaybe)

data Equation
    = Eqn
    { result :: Int
    , terms :: [Int]
    }

-- TODO: A generic version working in reverse ?

workingInReversePart1 :: [Equation] -> Int
workingInReversePart1 eqns =
    sum $ part1' <$> eqns
  where
    part1' :: Equation -> Int
    part1' Eqn{result = r, terms = ints} =
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

workingInReversePart2 :: [Equation] -> Int
workingInReversePart2 eqns =
    sum $ part2' <$> eqns
  where
    cutSuffix :: String -> String -> Maybe (String, String)
    cutSuffix toRemove from =
        let suffix = reverse . take (length toRemove) $ reverse from
            prefix = take (length from - length suffix) from
            verify = suffix == toRemove
         in if verify then Just (suffix, prefix) else Nothing
    toInt :: String -> Maybe Int
    toInt = readMaybe
    part2' :: Equation -> Int
    part2' Eqn{result = r, terms = ints} =
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
            -- if c3 is 0, it means that we failed at cutting the suffix anyway
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

-- INFO: the code below is valid but twice slower !
--    nextRun = flip compute (b : xs)
-- in modC1 /= 0 && nextRun c1 || c2 <= 0 && nextRun c2 || maybeSuffix == ("", "") && nextRun c3

day7 :: T.Text -> (Int, Int)
day7 input =
    let values = parse input
     in (workingInReversePart1 values, workingInReversePart2 values)

parse :: T.Text -> [Equation]
parse input = toEquation <$> T.lines input
  where
    toInt :: T.Text -> Int
    toInt = read . T.unpack
    toEquation :: T.Text -> Equation
    toEquation i =
        let components = T.splitOn (T.pack ":") i
            rawResult =
                case components of
                    (x : _) -> x
                    _ -> error "Parsing should not fail !"
            rawOperands1 =
                case components of
                    (_ : xs) -> T.strip $ T.concat xs
                    _ -> error "Parsing should not failt !"
            rawOperands2 = T.splitOn (T.pack " ") rawOperands1
         in Eqn{result = toInt rawResult, terms = toInt <$> rawOperands2}

-- INFO: a more generic version twice slower than specific ones...

-- TODO: Why is it slower ?
genericWorkingInOrderPart1 :: [Equation] -> Int
genericWorkingInOrderPart1 eqns =
    sum $ genericWorkingInOrderSolver [Mul, Add] <$> eqns

genericWorkingInOrderPart2 :: [Equation] -> Int
genericWorkingInOrderPart2 eqns =
    sum $ genericWorkingInOrderSolver [Mul, Add, Concat] <$> eqns

genericWorkingInOrderSolver :: [Int -> Int -> Op] -> Equation -> Int
genericWorkingInOrderSolver ops Eqn{result = total, terms = terms} =
    if compute 0 terms then total else 0
  where
    compute :: Int -> [Int] -> Bool
    compute ir [] =
        total == ir
    compute !ir [a] =
        elem total $ opToValue . (\x -> x ir a) <$> ops
    compute 0 (a : b : xs) =
        let cx = (\op -> op a b) <$> ops
            values = opToValue <$> cx
         in any (`compute` xs) values
    compute !ir (a : b : xs) =
        let cx = (\op -> op ir a) <$> ops
            values = opToValue <$> cx
            run = (toto <$> values)
            toto c = c < total && compute c (b : xs)
         in or run

-- INFO: Less efficient solution than working in reverse, still way more efficient than the Tree solution
workingInOrderPart1 :: [Equation] -> Int
workingInOrderPart1 eqns =
    sum $ solver <$> eqns
  where
    solver :: Equation -> Int
    solver Eqn{result = total, terms = terms} =
        if compute 0 terms then total else 0
      where
        compute :: Int -> [Int] -> Bool
        compute ir [] =
            total == ir
        compute ir [a] =
            total == a + ir || total == a * ir
        compute 0 (a : b : xs) =
            let c1 = a + b
                c2 = a * b
             in compute c1 xs || compute c2 xs
        compute ir (a : b : xs) =
            let c1 = a + ir
                c2 = a * ir
             in c1 < total && compute c1 (b : xs) || c2 < total && compute c2 (b : xs)

workingInOrderPart2 :: [Equation] -> Int
workingInOrderPart2 eqns =
    sum $ solver <$> eqns
  where
    solver :: Equation -> Int
    solver Eqn{result = total, terms = terms} =
        if compute 0 terms then total else 0
      where
        compute :: Int -> [Int] -> Bool
        compute ir [] =
            total == ir
        compute ir [a] =
            total == a + ir || total == a * ir || show a ++ show ir == show total
        compute 0 (a : b : xs) =
            let c1 = a + b
                c2 = a * b
                c3 = read $ show a ++ show b
             in compute c1 xs || compute c2 xs || compute c3 xs
        compute ir (a : b : xs) =
            let c1 = a + ir
                c2 = a * ir
                c3 = read $ show a ++ show b
             in c1 < total && compute c1 (b : xs)
                    || c2 < total && compute c2 (b : xs)
                    || c3 < total && compute c3 (b : xs)

-- INFO: old inefficient Tree solution

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
getNumberOfSolvedPart1 eqn@Eqn{result = r, terms = p} =
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
getNumberOfSolvedPart2 eqn@Eqn{result = r, terms = p} =
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
