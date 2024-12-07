module AOCDay7 (day7) where

import Data.Text qualified as T

data Equation
    = Eqn
    { result :: Int
    , operands :: [Int]
    }
    deriving (Show)

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
day7 :: T.Text -> (Int, Int)
day7 input =
    let values = parse input
        part1 =
            sum . map (\(x, _) -> result x) . filter (\(_, Solved n) -> n > 0) $
                getNumberOfSolvedPart1 <$> values
        part2 =
            sum . map (\(x, _) -> result x) . filter (\(_, Solved n) -> n > 0) $
                getNumberOfSolvedPart2 <$> values
     in (part1, part2)

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
