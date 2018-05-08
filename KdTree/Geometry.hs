module KdTree.Geometry
( Point2D (..)
, Point3D (..)
, GeometryPoint (..)
, BoundingBox
, Point (..) 
, Geometry (..)
{-, Ray
, Triangle
, makeRay
, makeTriangle-}
, fromPoint
, module KdTree.Alg
, module Data.Binary
, module Data.Maybe
) where

import KdTree.Alg

import Data.Binary
import Data.Function
import Data.Maybe

data Point2D = Point2D Float Float       deriving (Show, Eq) 
data Point3D = Point3D Float Float Float deriving (Show, Eq)

newtype GeometryPoint a = GeometryPoint { getPoint :: a } deriving (Show, Eq)

fromPoint :: Point a => a -> GeometryPoint a
fromPoint = GeometryPoint

data BoundingBox a = Box 
                    { vMin :: a
                    , vMax :: a
                    } deriving (Show, Eq)

class (Show a, Eq a) => Point a where
    hPoint               :: (Float -> Float -> Float) -> a -> a -> a
    minPoint             :: a -> a -> a
    minPoint = hPoint min
    maxPoint             :: a -> a -> a
    maxPoint = hPoint max
    boundingBoxFromPoint :: a -> BoundingBox a
    boundingBoxFromPoint a = Box a a
    pointInBox           :: BoundingBox a -> a -> Bool
    addPointInBox        :: BoundingBox a -> a -> BoundingBox a
    splitBoxWithPoints   :: Int -> (BoundingBox a, [a]) -> IO ((BoundingBox a, [a]), (BoundingBox a, [a]))
    intersectionBox      :: BoundingBox a -> BoundingBox a -> Maybe (BoundingBox a)
    makeBox              :: a -> a -> BoundingBox a
    makeBox p = addPointInBox $ boundingBoxFromPoint p
    pointBoxDist         :: (a -> a -> Float) -> a -> BoundingBox a -> Float
    generatePoints       :: RandomGen g => BoundingBox a -> g -> [a] -- infinity list
    printPoints          :: [a] -> IO ()
    printPoints = putStr . unlines . map (unwords . tail . words . show)
    euclideanDistance    :: a -> a -> Float


class Geometry a where
    boundingBoxFromOne :: Point p => a p -> BoundingBox p
    addInBoundingBox   :: Point p => BoundingBox p -> a p -> BoundingBox p
    boundingBoxFrom    :: Point p => [a p] -> BoundingBox p
    boundingBoxFrom []       = error "cannot create empty bounding box"
    boundingBoxFrom (x : xs) = foldl addInBoundingBox (boundingBoxFromOne x) xs
    splitBoundingBox   :: Point p => Int -> (BoundingBox p, [a p]) ->
                                IO ((BoundingBox p, [a p]), (BoundingBox p, [a p]))

instance Binary Point2D where
    put (Point2D x y) = do
        put x
        put y
    get = Point2D <$> get <*> get

instance Binary Point3D where
    put (Point3D x y z) = do
        put x
        put y
        put z
    get = Point3D <$> get <*> get <*> get

instance Binary a => Binary (BoundingBox a) where
    put (Box x y) = do
        put x
        put y
    get = Box <$> get <*> get

instance Binary a => Binary (GeometryPoint a) where
    put = put . getPoint
    get = GeometryPoint <$> get

instance Point Point2D where    
    hPoint f (Point2D x1 y1) (Point2D x2 y2) = Point2D (f x1 x2) (f y1 y2)

    pointInBox (Box (Point2D x1 y1) (Point2D x2 y2)) (Point2D x y) =
        x >= x1 && x <= x2 && y >= y1 && y <= y2

    addPointInBox (Box p1 p2) x = 
        Box (minPoint x p1) (maxPoint x p2)

    splitBoxWithPoints n (_, points) = do
        median <- findMedian $ map p points
        return $ on (on (,) (\x -> (boundingBoxFrom $ map GeometryPoint x, x))) 
            (\func -> filter (func median) points) less great
        where (p, less, great) =
                if mod n 2 == 0
                    then (\(Point2D x _) -> x, \m (Point2D x _) -> x <= m, \m (Point2D x _) -> x > m) 
                    else (\(Point2D _ y) -> y, \m (Point2D _ y) -> y <= m, \m (Point2D _ y) -> y > m)

    intersectionBox (Box p1 p2) (Box p3 p4)
        | maxX >= minX && maxY >= minY = Just $ Box x y
        | otherwise                    = Nothing
        where x@(Point2D minX minY) = maxPoint p1 p3
              y@(Point2D maxX maxY) = minPoint p2 p4

    pointBoxDist metric p@(Point2D x y) (Box (Point2D x1 y1) (Point2D x2 y2)) = 
        metric p . Point2D (max x1 $ min x x2) . max y1 $ min y y2

    generatePoints (Box (Point2D minX minY) (Point2D maxX maxY)) = h 
        where h gen = Point2D x y : h gen2
                where (x, gen1) = randomR (minX, maxX) gen
                      (y, gen2) = randomR (minY, maxY) gen1 

    euclideanDistance (Point2D x1 y1) (Point2D x2 y2) = sqrt $ on (+) (^2) (x1-x2) (y1-y2)

instance Point Point3D where
    hPoint f (Point3D x1 y1 z1) (Point3D x2 y2 z2) = Point3D (f x1 x2) (f y1 y2) (f z1 z2)

    pointInBox (Box (Point3D x1 y1 z1) (Point3D x2 y2 z2)) (Point3D x y z) =
        x >= x1 && x <= x2 && y >= y1 && y <= y2 && z >= z1 && z <= z2 

    addPointInBox (Box p1 p2) a = 
        Box (minPoint a p1) (maxPoint a p2)

    splitBoxWithPoints n (_, points) = do
        median <- findMedian $ map p points
        return $ on (on (,) (\x -> (boundingBoxFrom $ map GeometryPoint x, x))) 
            (\func -> filter (func median) points) less great
        where (p, less, great) =
                case (mod n 3) of
                    0 -> (\(Point3D x _ _) -> x, \m (Point3D x _ _) -> x <= m, \m (Point3D x _ _) -> x > m) 
                    1 -> (\(Point3D _ y _) -> y, \m (Point3D _ y _) -> y <= m, \m (Point3D _ y _) -> y > m)
                    _ -> (\(Point3D _ _ z) -> z, \m (Point3D _ _ z) -> z <= m, \m (Point3D _ _ z) -> z > m)

    intersectionBox (Box p1 p2) (Box p3 p4)
        | maxX >= minX && maxY >= minY && maxZ >= minZ = Just $ Box x y
        | otherwise                                    = Nothing
        where x@(Point3D minX minY minZ) = maxPoint p1 p3
              y@(Point3D maxX maxY maxZ) = minPoint p2 p4

    pointBoxDist metric p@(Point3D x y z) (Box (Point3D x1 y1 z1) (Point3D x2 y2 z2)) = 
        metric p . Point3D (f x1 x x2) (f y1 y y2) $ f z1 z z2
        where f = \a b c -> max a $ min b c 

    generatePoints (Box (Point3D minX minY minZ) (Point3D maxX maxY maxZ)) = h 
        where h gen = Point3D x y z : h gen3
                where (x, gen1) = randomR (minX, maxX) gen
                      (y, gen2) = randomR (minY, maxY) gen1
                      (z, gen3) = randomR (minZ, maxZ) gen2
      
    euclideanDistance (Point3D x1 y1 z1) (Point3D x2 y2 z2) = 
        sqrt . sum $ map (^2) [x1-x2, y1-y2, z1-z2]                  

instance Geometry GeometryPoint where
    boundingBoxFromOne   (GeometryPoint x) = boundingBoxFromPoint x
    addInBoundingBox box (GeometryPoint x) = addPointInBox box x
    splitBoundingBox h (box, list) = 
        fmap (uncurry $ on (,) (\(a, b) -> (a, map fromPoint b))) $ 
            splitBoxWithPoints h (box, map getPoint list)

{-newtype Ray a = Ray a deriving(Show)

makeRay :: Point p => p -> Ray p
makeRay = Ray

newtype Triangle a = Triangle (a, a, a) deriving(Show, Eq)

makeTriangle :: Point p => p -> p -> p -> Triangle p
makeTriangle a b c = Triangle (a, b, c)

intersectionTriangleWithBox :: Point p => Box p -> Triangle p -> Bool
intersectionTriangleWithBox 

instance Geometry Triangle where
    boundingBoxFromOne   (Triangle (a, b, c)) = foldl addPointInBox (boundingBoxFromPoint c) [a, b]
    addInBoundingBox box (Triangle (a, b, c)) = foldl addPointInBox box [a, b, c]
    splitBoundingBox h (box, triangles) =-}



