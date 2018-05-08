module KdTree.KdTree 
( KdTree
, kdFromList
, kdFromListOfPoints
, kdFindPointsInBox
, kdNearestNeighborSearch
, saveKdTree
, loadKdTree
, module KdTree.Geometry
) where

import KdTree.Geometry

import Data.List
import Data.Function
import Control.Monad
import Control.Applicative
import Data.Binary

data KdTree a b = Node b (KdTree a b) (KdTree a b) | Leaf b [a] deriving (Show, Eq)

maxHight :: Int
maxHight = 30

maxElementsInLeaf :: Int
maxElementsInLeaf = 5

getCoveringGeometry :: Point p => KdTree a (b p) -> b p
getCoveringGeometry (Leaf x _)    = x
getCoveringGeometry (Node x _ _ ) = x

kdFromList :: (Geometry a, Point p) => [a p] -> IO (KdTree (a p) (BoundingBox p))
kdFromList l = build 0 (boundingBoxFrom l, l) 
    where   build :: (Geometry a, Point p) => Int -> (BoundingBox p, [a p]) -> IO (KdTree (a p) (BoundingBox p))
            build hight (box, geometries) 
                | hight == maxHight || length geometries <= maxElementsInLeaf = return $ Leaf box geometries
            build hight x@(box, _) = do
                (left, right) <- join $ (\(lhs, rhs) -> do 
                        l <- f lhs
                        r <- f rhs
                        return (l, r)) <$> splitBoundingBox hight x
                return $ Node box left right
                where f = build $ hight + 1

kdFromListOfPoints :: Point p => [p] -> IO (KdTree (GeometryPoint p) (BoundingBox p))               
kdFromListOfPoints = kdFromList . map fromPoint

kdFindPointsInBox :: Point p => BoundingBox p -> KdTree (GeometryPoint p) (BoundingBox p) -> [p]
kdFindPointsInBox box = h $ Just box
    where h Nothing _                           = []
          h (Just box1) (Leaf box2 points)      = 
            fromMaybe [] $ filter <$> 
                (pointInBox <$> intersectionBox box1 box2) <*> (Just $ map getPoint points)
          h (Just box1) (Node box2 lNode rNode) = 
            on (++) (h $ intersectionBox box1 box2) lNode rNode


kdNearestNeighborSearch :: Point p => (p -> p -> Float) -> p -> KdTree (GeometryPoint p) (BoundingBox p) -> p
kdNearestNeighborSearch metric point tree = 
    fromJust $ neighborSearch (fromIntegral (maxBound :: Int) {-infinity-}) tree
    where   pointBoxDist1               = pointBoxDist metric point
            pointInBox1                 = flip pointInBox point
            leftBoxIsMoreCloseToPoint   = on (<) (pointBoxDist1 . getCoveringGeometry)
            neighborSearch dist1 (Leaf _ x) = if dist1 < dist2 then Nothing else Just candidate
                where points             = map getPoint x
                      (candidate, dist2) = minimumBy (on compare snd) . zip points $ map (metric point) points
            neighborSearch dist (Node box _ _) 
                | pointInBox1 box == False && pointBoxDist1 box >= dist = Nothing
            neighborSearch dist (Node _ lNode rNode) =
                if (pointInBox1 $ getCoveringGeometry lNode) || leftBoxIsMoreCloseToPoint lNode rNode
                    then query lNode rNode
                    else query rNode lNode
                where query lhs rhs = if isNothing point2 then point1 else point2
                        where point1 = neighborSearch dist lhs
                              point2 = neighborSearch (fromMaybe dist $ metric point <$> point1) rhs


instance (Binary a, Binary b) => Binary (KdTree a b) where
    put (Leaf box geometries) = do
        put True
        put box
        putList geometries
    put (Node box lNode rNode) = do
        put False
        put box
        put lNode
        put rNode
    get = do
        flag <- get
        if flag
            then Leaf <$> get <*> get
            else Node <$> get <*> get <*> get

saveKdTree :: (Binary a, Binary b) => String -> KdTree a b -> IO ()             
saveKdTree = encodeFile

loadKdTree :: (Binary a, Binary b) => String -> IO (KdTree a b)
loadKdTree = decodeFile