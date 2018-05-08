import Data.Function
import Data.List
import Control.Monad
import Data.Tuple
import Data.Array

type Rect = (Int, Int, Int, Int)


{-
    всеки правоъгълник го пресичам с всички правоъгълници
    включително и със себе си (затова изваждам 1)
    сортирам по брои пресичания
    от тези с най-гомятям брои пресичания
    взимам максималният по площ
-}

mostPopular :: [Rect] -> Rect
mostPopular list = 
    maximumBy (on compare area) . map snd . head . groupBy (((==) EQ .) . comp) . sortBy comp $ 
        map (\rect -> (flip (-) 1 . length $ filter (intersection rect) list, rect)) list
    where   comp = flip $ on compare fst
            intersection :: Rect -> Rect -> Bool
            intersection (x1, y1, x2, y2) (x3, y3, x4, y4) = 
                minX <= maxX && minY <= maxY 
                where   minX = max x1 x3
                        minY = max y1 y3
                        maxX = min x2 x4
                        maxY = min y2 y4
            area :: Rect -> Int
            area (x1, y1, x2, y2) = (x2 - x1) * (y2 - y1)

mostPopularTest1 = mostPopular [(4,3,11,8),(7,0,13,6),(2,2,8,5),(0,4,6,7)] == (4,3,11,8)
mostPopularTest2 = (==) (0,0,10,10) . mostPopular . (++) [(0,0,9,9),(0,0,10,10)] $ replicate 20 (2,2,3,3)

{-
    вземам комбинациите на фунцтиите
    премахвам нулевата, която е първа
    перутирам всяка комбинация
    това са всички вериги от функции

    премахвам тези, които не отговарят на условието
    и връщам дължината на максималната верига
    
    гарантирам си, че ако няма такава верига ще върна 0
-}



check :: [(Int -> Int)] -> [Int] -> Int
check functions numbers = maximum . (:) 0 . map length . concat .
    map (\(f, listFChain) -> filter (\fChain -> all (liftM2 (==) f (\x -> foldr ($) x fChain)) numbers) listFChain) $ 
        map (liftM2 (,) 
            (head . snd) 
                (concat . map permutations . tail . subsequences . liftM2 (++) fst (tail . snd)) . 
                    flip splitAt functions) 
                        [0 .. length functions - 1]


checkTest1 = check [ (+1), (^2), (*3), (div 2), \x -> 3*x^2 + 1 ] [1..100] == 3

uniq :: Ord a => [a] -> [a]
uniq = map head . group . sort

{-една точка може да е и минимална и максимална в реда си-}

{-
Бавният вариант
allSaddles :: (Num a, Ord a) => [[a]] -> [(Int,Int)]
allSaddles matrix =
    uniq $ liftM2 (on (++)
        (\(f, g) -> concat $
            map (\(index, row) -> 
                let val = f row 
                in zip (repeat index) . map fst . filter ((==) val . g . snd) . 
                    liftM2 zip id (map $ (!!) tMatrix) $ elemIndices val row) indMatrix))
                        id swap (minimum, maximum)
    where   indMatrix = zip [0..] matrix 
            tMatrix   = transpose matrix-}

allSaddles :: (Num a, Ord a) => [[a]] -> [(Int,Int)]
allSaddles matrix = 
        map (indexToRowCol . fst) . filter 
            (\(ind, val) -> 
            let (x, y) = indexToRowCol ind 
            in  on (&&) (val==) (rowIndicesMax ! x) (colIndicesMin ! y) || 
                on (&&) (val==) (rowIndicesMin ! x) (colIndicesMax ! y)) . zip [0..] $ concat matrix
    where   tMatrix                        = transpose matrix
            (row, col)                     = on (,) length matrix tMatrix
            (rowIndicesMin, rowIndicesMax) = on (,) (listArray (0, row - 1) . flip map matrix)  minimum maximum
            (colIndicesMin, colIndicesMax) = on (,) (listArray (0, col - 1) . flip map tMatrix) minimum maximum
            indexToRowCol :: Int -> (Int, Int)
            indexToRowCol n = (r, n - r*col)
                where r = div n col

hasSaddle :: (Num a, Ord a) => [[a]] -> Bool
hasSaddle = not . null . allSaddles 

allSaddlesTest1 = allSaddles [[1,2,3],[5,3,5],[0,0,0]] == [(1,1),(2,0),(2,1),(2,2)]
allSaddlesTest2 = allSaddles [[1,2,3],[0,2,3],[2,2,2]] == [(2,0),(2,1),(2,2)]
allSaddlesTest3 = allSaddles [[1,2,3],[0,2,3],[2,1,5]] == [(0,2),(1,2)]
allSaddlesTest4 = allSaddles [[4,3,4,5],[2,0,4,3]]     == [(0,1),(1,2)]

primes :: [Int]
primes = [ x | x <- [2..], all ((/=) 0 . mod x) [2 .. div x 2] ]   

reprs1 :: [Int]
reprs1 = 
    map (\x -> 
        let primeNum = takeWhile (<x) primes
        in (+) (length $ filter ((==) x . (2*)) primeNum) . flip div 2 . length $ filter (flip elem primeNum . (-) x) primeNum) [0..]

reprs :: [Int]
reprs =
    map (\x ->
        let list     = takeWhile (<x) primes
            len      = length list - 1
            primeNum = listArray (0, len) list
            help lhs rhs 
                | lhs > rhs    = 0
                | x == currSum = 1 + help (lhs + 1) (rhs - 1)
                | x >  currSum =     help (lhs + 1) rhs
                | otherwise    =     help lhs $ rhs - 1
                where currSum = on (+) (primeNum !) lhs rhs
        in help 0 len) [0..]

reprsTest n = on (==) (take n) reprs1 reprs        

data DFA = DFA Int [Int] (Int -> Char -> Int)

trans :: Int -> Char -> Int
trans 0 'a' = 1
trans 0 'b' = 0
trans 0 'c' = 0
trans 1 'a' = 0
trans 1 'b' = 1
trans 1 'c' = 1
trans _  _  = -1

match :: DFA -> String -> Bool
match (DFA 0 _ _)              = const False
match (DFA _ terminalStates g) = match1 0
    where   match1 :: Int -> String -> Bool
            match1 state []       = elem state terminalStates
            match1 state (x : xs) = flip match1 xs $ g state x

dfa1 :: DFA
dfa1 = DFA 2 [0] trans


{-този автомат разпознава всички думи с дължина поне 4-}
dfa2 :: DFA
dfa2 = DFA 5 [4] (const . min 4 . (+1))

dfaTest =
    all (liftM2 (==) (match dfa1 . fst) snd) 
        [("abcbcab",True),("baba",True),("baobab",False),("aaaaa",False)]

data NFA = NFA [Int] (Int -> Char -> [Int])

matchNFA :: NFA -> String -> Bool
matchNFA (NFA terminalStates g) = match1 0
    where   match1 :: Int -> String -> Bool
            match1 state []       = elem state terminalStates
            match1 state (x : xs) = any (flip match1 xs) $ g state x


trans1 :: Int -> Char -> [Int]
trans1 0 'a' = [0,1,2,-1]
trans1 0 'b' = [0,-1]
trans1 0 'c' = [0,-1]
trans1 1 'a' = [0,1]
trans1 1 'b' = [1,9]
trans1 1 'c' = [1..4]
trans1 _  _  = [-1]

testNFA =
    all (liftM2 (==) (matchNFA (NFA [0] trans1) . fst) snd) 
        [("abcbcab",True),("baba",True),("baobab",False),("aaaaa",True)]