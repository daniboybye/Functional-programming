import Data.List hiding(permutations)
import Data.Function

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x : xs) = concat . map (\list -> 
        map (\(l, r) -> (++) l $ x : r) $
            map (\index -> splitAt index list) [0 .. len] ) $ permutations xs
    where len = length xs

type Point = (Int, Int)

dist :: (Point, Point) -> Int
dist ((ax, ay), (bx, by)) = round .  sqrt . fromIntegral . on (+) (^2) (ax - bx) $ ay - by


--губя точност заради Int-та
alcoholismPath :: Point -> [Point] -> Int
alcoholismPath _ [] = 0
alcoholismPath home bars = minimum . map (\x -> sum . map dist . zip (home : x) $ x ++ [home]) $ permutations bars