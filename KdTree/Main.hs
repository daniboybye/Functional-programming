module Main where

import KdTree.KdTree

import Data.List
import Control.Monad
import System.Directory
import Data.Function
import Data.Time.Clock

testKdTreeSerialization :: IO ()
testKdTreeSerialization = do
    kd2D         <- join $ kdFromListOfPoints <$> gen (makeBox (Point2D 0 0) $ Point2D 100 100)
    kd3D         <- join $ kdFromListOfPoints <$> gen (makeBox (Point3D 0 0 0) $ Point3D 1000 1000 1000)
    beginTime    <- getCurrentTime
    saveKdTree fileName2D kd2D
    saveKdTree fileName3D kd3D
    kd2DfromFile <- loadKdTree fileName2D
    kd3DfromFile <- loadKdTree fileName3D
    endTime      <- getCurrentTime
    putStr "Тime for serialization and desertification: "
    print $ diffUTCTime endTime beginTime
    removeFile fileName2D
    removeFile fileName3D
    print $ kd2DfromFile == kd2D && kd3DfromFile == kd3D 
    where gen box     = take 1000 <$> generatePoints box <$> getStdGen
          fileName2D  = "KdTree\\serialization2D.bin"
          fileName3D  = "KdTree\\serialization3D.bin"

testKdNearestNeighborSearch :: IO ()
testKdNearestNeighborSearch = do
    x : xs    <- take numberOfPoints <$> generatePoints (makeBox (Point2D 0 0) (Point2D 100 100)) <$> getStdGen
    let nearestNeighborPoint1 = fst . minimumBy (on compare snd) . zip xs $ map (euclideanDistance x) xs
    tree      <- kdFromListOfPoints xs
    beginTime <- getCurrentTime
    let nearestNeighborPoint2 = kdNearestNeighborSearch euclideanDistance x tree
    endTime   <- getCurrentTime
    putStr $ "Time for query nearest neighbor for tree with " ++ show numberOfPoints ++ " points: "
    print $ diffUTCTime endTime beginTime
    print $ nearestNeighborPoint1 == nearestNeighborPoint2
    where numberOfPoints = 10000
    {-print $ euclideanDistance x nearestNeighborPoint1
    print nearestNeighborPoint1
    print $ euclideanDistance x nearestNeighborPoint2
    print nearestNeighborPoint2-}
    
testKdPointsInBox :: IO ()
testKdPointsInBox = do
    x : y : points <- take numberOfPoints <$> generatePoints (makeBox (Point3D 0 0 (-10.6)) (Point3D 100 1000 54.4)) <$> getStdGen
    let box = makeBox x y
    tree                <- kdFromListOfPoints points
    beginTime           <- getCurrentTime
    let pointsInBoxFromTree = kdFindPointsInBox box tree
    endTime             <- getCurrentTime
    putStr $ "Time for query \"which points are in the box\" for tree with " ++ show numberOfPoints ++ " points: "
    print $ diffUTCTime endTime beginTime
    print . and . on (zipWith (==)) (sortBy comp) pointsInBoxFromTree $ filter (pointInBox box) points
    where numberOfPoints = 100000
          comp (Point3D x1 y1 z1) (Point3D x2 y2 z2) 
            | x1 /= x2  = compare x1 x2
            | y1 /= y2  = compare y1 y2
            | otherwise = compare z1 z2  

main :: IO ()
main = do
    --testKdTreeSerialization
    --testKdNearestNeighborSearch
    testKdPointsInBox
    
