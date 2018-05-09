module SpamFilter.Queue
( Queue
, empty
, isEmpty
, enqueue
, dequeue
) where

data Queue a = Queue [a] [a] deriving (Show, Eq)

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

enqueue :: a -> Queue a -> Queue a
enqueue y (Queue xs ys) = Queue xs (y:ys)

dequeue :: Queue a -> (a, Queue a)
dequeue (Queue [] (x:xs)) = (x, Queue (reverse xs) [])
dequeue (Queue (x:xs) ys) = (x, Queue xs ys)