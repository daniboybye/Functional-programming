import Data.List
import Data.Function
import Data.Tuple
import Data.Maybe
import Control.Monad
import Control.Applicative

import qualified Data.Array as Array
import qualified Data.Map.Strict as Map

type Time = (Int,Int)
type TVShow = (String, Time,Int)

{-
    Шоу (Име, Начало, Продължителност)

    Да се напише функция, която по даден
    спицък от предавания ренерира възможно най-дългата
    телевизионна програма (сумата от продължителностите е максимална)
-}

longestProgram :: [TVShow] -> [TVShow]
longestProgram list = 
    indicesToShows . reverse . snd . snd . Map.findMax . foldl' 
        (\stateTable (index, lshow) ->
        liftM2 (Map.insert $ getEndTime lshow) 
            (liftM2 (max . snd)
                Map.findMax
                    (liftM2 (,) 
                        ((+) (getLen lshow) . fst) 
                            ((:) index . snd) . snd . fromJust . Map.lookupLE (getBeginTime lshow))) 
                                id 
                                    stateTable)
        (Map.singleton 0 (0, [])) $ Array.assocs array
    where   getBeginTime   = \(_, (h, m), _) -> h*60 + m
            getLen         = \(_, _, len) -> len
            getEndTime     = liftM2 (+) getBeginTime getLen
            array          = Array.listArray (0, length list - 1) $ sortOn getEndTime list
            indicesToShows = map ((Array.!) array)

shows1 :: [TVShow]                
shows1 = [("A",(11,0),120),("B",(12,0),15),("C",(10,30),90)]

shows2 :: [TVShow]
shows2 = [("A",(11,0),120),("B",(12,0),15),("C",(10,30),90),("D",(5,50),110),("E",(7,55),45)]

shows3 :: [TVShow]
shows3 = concat [shows1, shows2, [("F",(8,10),143)]]

test1 = longestProgram shows1
test2 = longestProgram shows2
test3 = longestProgram shows3

shows4 = [("A",(11,0),120),("B",(12,0),15),("C",(10,30),90), ("D", (8,0), 200)]
test4  = longestProgram shows4 == [("D",(8,0),200), ("B",(12,0),15)]