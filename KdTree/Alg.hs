module KdTree.Alg
( findKthElement
, findMedian
, module System.Random
) where

import System.Random

findKthElement :: (Num a, Ord a) => Int -> [a] -> IO a
findKthElement index list = do
    gen <- getStdGen
    return $ findKthElement1 gen index (length list) list
    where   findKthElement1 :: (Num a, Ord a) => StdGen -> Int -> Int -> [a] -> a
            findKthElement1 _   _     _   [x]  = x
            findKthElement1 gen index len list = 
                if lenLeft > index 
                    then f index             lenLeft         left
                    else f (index - lenLeft) (len - lenLeft) right
                where (pivot, f) = 
                        (\(pivotIndex, newGen) -> 
                            (list !! pivotIndex, findKthElement1 newGen)) $ 
                            randomR (0, len - 1) gen
                      left       = filter (<= pivot) list
                      right      = filter (>  pivot) list
                      lenLeft    = length left

findMedian :: (Num a, Ord a) => [a] -> IO a
findMedian list = flip findKthElement list . flip div 2 $ length list