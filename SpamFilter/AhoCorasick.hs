module SpamFilter.AhoCorasick
( AhoCorasick
, Vertex
, EndWords
, WordID
, AcChar
, AcString
, root
, fromList
, matching
) where

import SpamFilter.AhoCorasickTypes
import qualified Data.Map.Lazy as Map

data AhoCorasick = AC GoTo Failure Out deriving (Show)

insertStrInTrie :: Vertex -> Key -> (Out1, GoTo) -> (Out1, GoTo)
insertStrInTrie state (iD, []) (out, goTo) = 
    (outInsert1 state iD out, goTo)

insertStrInTrie currState (iD, firstChar : string) (out, goTo) =
    insertStrInTrie (fromMaybe newState state) (iD, string) $
        if isNothing state
            then (outInsert1 newState defEndWords out, goToInsert key newState goTo)
            else (out, goToInsert key (fromJust state) goTo)
    where key      = (currState, firstChar)
          newState = nameNewState goTo
          state    = goToFind key goTo
          

fromListTrie :: [Key] -> (Out1, GoTo)
fromListTrie = foldr (insertStrInTrie root) (outEmpty1, goToEmpty)


nextState :: GoTo -> Failure -> Vertex -> AcChar -> Vertex
nextState goTo failure state char = 
    fromMaybe root $ goToFind (until 
        (\x -> root == x || goToMember (x, char) goTo) 
            (`failureFind` failure) state, char) goTo

nextState1 :: GoTo -> Failure1 -> Vertex -> AcChar -> Vertex
nextState1 goTo failure state char = 
    fromMaybe root $ goToFind (until 
        (\x -> root == x || goToMember (x, char) goTo) 
            (`failureFind1` failure) state, char) goTo

createFailureOut :: (Out1, GoTo) -> (Failure1, Out1)
createFailureOut (o, goTo) =
    bfs (foldr enqueue empty firstLevel, {- make queue -} 
        foldr (flip failureInsert1 root) failureEmpty1 firstLevel, {- make failture -} 
            o)
    where   bfs :: (Queue Vertex, Failure1, Out1) -> (Failure1, Out1)
            bfs (queueA, failureA, outA) | isEmpty queueA = (failureA, outA)
            bfs (queueA, failureA, outA) =
                bfs . foldr 
                    (\(state, char) (queue, failure, out) ->
                    let failLink = nextState1 goTo failure currStateFailLink char
                    in (enqueue state queue, {- new queue -}
                        failureInsert1 state failLink failure, {- new failure -}
                        outInsert1 state (on addEndWords (`getOut1` out) state failLink) out) {- new out -} ) 
                    (queueS, failureA, outA) . map (liftM2 (,) (fromJust.fst) snd) . filter (isJust.fst) $ 
                        map (\char -> (goToFind (currState, char) goTo, char)) alphabet
                where   (currState, queueS) = dequeue queueA
                        currStateFailLink   = failureFind1 currState failureA
            firstLevel = map fromJust . filter isJust $ map (\x -> goToFind (root, x) goTo) alphabet
          

fromList :: [Key] -> AhoCorasick
fromList strs = 
    uncurry (AC goTo) . liftM2 (,) (failure1ToFailure . fst) (out1ToOut . snd) $ createFailureOut a
    where a@(_, goTo)    = fromListTrie strs

matching :: AhoCorasick -> AcString -> EndWords
matching (AC goTo failure output) = snd .
    foldl' (\(state, res) ->
            liftM2 (,) id (addEndWords res . flip getOut output) . nextState goTo failure state)
            (root, defEndWords)


strTest1  = flip zip ["dani","dan","ani","ni","nina"] $ map (\x -> [x]) [1 ..]

trieTest1 = 
    (Map.fromList [(1,[]),(2,[4]),(3,[]),(4,[5]),(5,[]),(6,[]),(7,[3]),(8,[]),(9,[]),(10,[2]),(11,[1])],
        Map.fromList [((0,'a') ,5),((0,'d'),8),((0,'n'),1),((1,'i'),2),((2,'n'),3),((3,'a'),4),((5,'n'),6),((6,'i'),7),((8,'a'),9),((9,'n'),10),((10,'i'),11)]) 
            == fromListTrie strTest1

test1 = (==) [1..5] . sort . flip matching "danina" $ fromList strTest1