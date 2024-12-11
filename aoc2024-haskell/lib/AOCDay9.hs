{-# LANGUAGE ScopedTypeVariables #-}

module AOCDay9 (day9, part2) where

import Control.Monad.ST (runST)
import Data.Char (digitToInt)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable qualified as MV

day9 :: T.Text -> (Int, Int)
day9 input =
    (part1 $ parse input, part2 $ parse input)

part1 :: V.Vector Int -> Int
part1 v = runST $ do
    v' <- V.thaw expanded
    runDefragmentation v' validIndiciesFromTheEnd 0
    compute <$> V.freeze v'
  where
    expanded = expand v
    compute = V.sum . V.imap (*) . V.filter (\x -> x /= (-1))
    validIndiciesFromTheEnd =
        reverse . V.toList $ V.findIndices (\x -> x /= (-1)) expanded
    runDefragmentation ::
        (MV.PrimMonad m, MV.MVector n Int) => n (MV.PrimState m) Int -> [Int] -> Int -> m ()
    runDefragmentation _ [] _ = return ()
    runDefragmentation mv yy@(y : ys) idx
        | idx == y = return ()
        | otherwise =
            if expanded V.! idx == -1
                then MV.write mv idx (expanded V.! y) >> MV.write mv y (-1) >> runDefragmentation mv ys (idx + 1)
                else runDefragmentation mv yy (idx + 1)

part2 :: V.Vector Int -> Int
part2 v =
    runST $ do
        v' <- V.thaw expanded
        runDefragmentation v' fileBlocks
        compute <$> V.freeze v'
  where
    compute = V.sum . V.imap (\i x -> if x < 0 then 0 else x * i)
    expanded = expand v
    fileBlocks = getFileBlocksFromTheEnd expanded

    runDefragmentation _ [] = return ()
    runDefragmentation mv (block : blocks) = do
        tmpVect <- V.freeze mv
        let freeBlocks = getConsecutiveFreeSpots' (head block) tmpVect
        let freeBlocksAvailable = find (\x -> length x >= length block) freeBlocks
        let freeBlock = fromMaybe [] freeBlocksAvailable
        if not . null $ freeBlock
            then write mv freeBlock block >> runDefragmentation mv blocks
            else runDefragmentation mv blocks

    write mv free file =
        let sizeOfFreeBlock = length free
            sizeOfFileBlock = length file
            instructions = zip file free
            newFree = drop (length file) free
         in if sizeOfFreeBlock < sizeOfFileBlock
                then error "This should not happen"
                else do
                    mapM_ (\(fileId, freeIdx) -> MV.write mv freeIdx (expanded V.! fileId)) instructions
                    mapM_ (\i -> MV.write mv i (-1)) file
                    if null newFree
                        then return ([], True)
                        else return (newFree, True)

getConsecutiveFreeSpots' :: Int -> V.Vector Int -> [[Int]]
getConsecutiveFreeSpots' before =
    foldr gFunc [] . V.toList . V.findIndices (== (-1)) . V.ifilter (\i _ -> i < before)
  where
    gFunc x [] = [[x]]
    gFunc x (ys@(y : _) : yss)
        | x + 1 == y = (x : ys) : yss
        | otherwise = [x] : ys : yss
    gFunc _ _ = error "This should not happen"

getConsecutiveFreeSpots :: V.Vector Int -> [[Int]]
getConsecutiveFreeSpots =
    foldr gFunc [] . V.toList . V.findIndices (== (-1))
  where
    gFunc x [] = [[x]]
    gFunc x (ys@(y : _) : yss)
        | x + 1 == y = (x : ys) : yss
        | otherwise = [x] : ys : yss
    gFunc _ _ = error "This should not happen"

getFileBlocksFromTheEnd :: V.Vector Int -> [[Int]]
getFileBlocksFromTheEnd v =
    reverse . foldr gFunc [] . V.toList . V.findIndices (\x -> x /= (-1) || x /= (-2)) $ v
  where
    gFunc x [] = [[x]]
    gFunc x (ys@(y : _) : yss)
        | v V.! x == v V.! y = (x : ys) : yss
        | otherwise = [x] : ys : yss
    gFunc _ _ = error "This should not happen"

expand :: V.Vector Int -> V.Vector Int
expand v =
    V.fromList $ doExpansion 0 0
  where
    doExpansion t idx
        | idx >= V.length v = []
        | otherwise =
            let blockFile = v V.! idx
                freeSpace = v V.!? (idx + 1)
             in case freeSpace of
                    Just fs -> writeBlocks t (blockFile, fs) ++ doExpansion (t + 1) (idx + 2)
                    Nothing -> writeBlocks t (blockFile, 0)

    writeBlocks :: Int -> (Int, Int) -> [Int]
    writeBlocks idx (blockFile, freeSpace) =
        if freeSpace == 0
            then
                replicate blockFile idx
            else
                replicate blockFile idx ++ replicate freeSpace (-1)

parse :: T.Text -> V.Vector Int
parse = V.fromList . map digitToInt <$> T.unpack . T.strip
