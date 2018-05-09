module SpamFilter.AhoCorasickTypes 
    ( module SpamFilter.AhoCorasickTypes
    , module Data.Maybe
    , module Data.List
    , module Data.Char
    , module Data.Int
    , module Data.Array
    , module Data.Function
    , module Control.Monad
    , module SpamFilter.Queue
    ) where

import Data.Maybe
import Data.List
import Data.Char
import Data.Int
import Data.Array
import Data.Function
import Control.Monad
import qualified Data.Map.Lazy as Map

import SpamFilter.Queue

type AcChar   = Char
type AcString = [AcChar]
type WordID   = Int16
type EndWords = [WordID]
type Key      = (EndWords, AcString)
type Vertex   = Int32

root :: Vertex
root = minBound :: Vertex

alphabet :: AcString
alphabet = ' ' : ['a' .. 'z']

defEndWords :: EndWords
defEndWords = []

addEndWords :: EndWords -> EndWords -> EndWords
addEndWords = (++)

type GoTo = Map.Map (Vertex, AcChar) Vertex

goToEmpty :: GoTo
goToEmpty = Map.empty

nameNewState :: GoTo -> Vertex
nameNewState = (+) (1 + root) . fromIntegral . Map.size

goToInsert :: (Vertex, AcChar) -> Vertex -> GoTo -> GoTo
goToInsert = Map.insert

goToFind :: (Vertex, AcChar) -> GoTo -> Maybe Vertex
goToFind = Map.lookup

goToMember :: (Vertex, AcChar) -> GoTo -> Bool
goToMember = Map.member

type Failure1 = Map.Map Vertex Vertex
type Failure  = Array Vertex Vertex

failureFind1 :: Vertex -> Failure1 -> Vertex
failureFind1 = (fromJust .) . Map.lookup

failureFind :: Vertex -> Failure -> Vertex
failureFind = flip (!)

failureEmpty1 :: Failure1
failureEmpty1 = Map.empty

failureToList1 :: Failure1 -> [(Vertex, Vertex)]
failureToList1 = Map.toList

failureSize1 :: Failure1 -> Int
failureSize1 = Map.size

failure1ToFailure :: Failure1 -> Failure
failure1ToFailure = liftM2 (\len -> listArray (root + 1, root + fromIntegral len) ) failureSize1 $ map snd . failureToList1

failureInsert1 :: Vertex -> Vertex -> Failure1 -> Failure1
failureInsert1 = Map.insert

type Out1 = Map.Map Vertex EndWords
type Out  = Array Vertex EndWords

outEmpty1 :: Out1
outEmpty1 = Map.empty

outInsert1 :: Vertex -> EndWords -> Out1 -> Out1
outInsert1 = Map.insert

outSize1 :: Out1 -> Int
outSize1 = Map.size

outToList1 :: Out1 -> [(Vertex, EndWords)]
outToList1 = Map.toList

out1ToOut :: Out1 -> Out
out1ToOut = liftM2 (listArray . (,) root . (+) root . fromIntegral) outSize1 $ (:) defEndWords . map snd . outToList1

getOut1 :: Vertex -> Out1 -> EndWords
getOut1 = Map.findWithDefault defEndWords

getOut :: Vertex -> Out -> EndWords
getOut = flip (!)