module SpamFilter.Table
    ( IntStat
    , FloatStat
    , Word(W)
    , Table(T)
    , fromTable
    , getID
    , getWord
    , getMatchesInSpam
    , getMatchesInNormal
    , getTable
    , getSpamEmails
    , getNormalEmails
    , isSpam
    , validateТheТext
    , updateTableWithNormalEmail
    , updateTableWithSpamEmail
    , module SpamFilter.AhoCorasick
    --, qualified module Set ???? https://wiki.haskell.org/GHC/QualifiedModuleExport
    ) where
    
import SpamFilter.AhoCorasick
import SpamFilter.Algorithm

import Data.Int
import qualified Data.Set as Set
import Data.Function
import Data.List
import Data.Maybe
import Data.Char
import Data.Binary hiding (Word)
import Prelude hiding (Word)
import Control.Monad

type FloatStat = Double
type IntStat   = Int16

data Word = W
    { getID              :: WordID
    , getWord            :: AcString
    , getMatchesInSpam   :: IntStat
    , getMatchesInNormal :: IntStat
    } deriving (Show)

instance Eq Word where  
    (==) = on (==) getID

instance Ord Word where
    compare = on compare getID

instance Binary Word where
    put (W iD str spam normal) = do
        put iD
        put str
        put spam
        put normal
    get = do 
        liftM4 W get get get get

data Table = T 
    { getTable         :: Set.Set Word
    , getSpamEmails    :: IntStat
    , getNormalEmails  :: IntStat
    } deriving (Show)

instance Binary Table where
    put (T table spam normal) = do
        putList $ Set.toList table
        put spam
        put normal
    get = liftM3 T (Set.fromList <$> get) get get

fromTable :: Table -> AhoCorasick
fromTable = fromList . map (\x -> ([getID x], (:) ' ' $ getWord x ++ [' '])) . Set.toList . getTable

validateТheТext :: AcString -> AcString
validateТheТext = (:) ' ' . flip (++) " " . unwords . map (map toLower) . words

findWordInTableById :: WordID -> Set.Set Word -> Word
findWordInTableById iD = fromJust . Set.lookupLE (W iD "" 0 0)

-- EndWords списъка трябва да е с уникални елементи

{-
    P(S|w) = P(w|S) * P(S) / P(w)
    P(N|w) = P(w|N) * P(N) / P(w)

    P(S|w)/P(N|w) > 1 is spam
    P(w|S) * P(S) / P(w|N) * P(N) > 1

    log2 (P(S|w)/P(N|w)) = log2 (P(S)/P(N)) + sum log2 (P(wi|S)/P(wi|N))
-}

isSpam :: Table -> EndWords -> Bool
isSpam (T table s n) = 
    (> 0) . (+) (log2 $ spam / normal) . sum .
        map ((\(x, y) -> log2 $ (x / spam) / (y / normal)) . liftM2 (on (,) fromIntegral) getMatchesInSpam getMatchesInNormal . 
            flip findWordInTableById table)
    where (spam, normal) = on (,) fromIntegral s n
          log2           = logBase 2

updateTableHelp :: (Word -> Word) -> Set.Set Word -> EndWords -> Set.Set Word
updateTableHelp h = foldl' (\table -> flip Set.insert table . h . flip findWordInTableById table)

updateTableWithNormalEmail :: Table -> EndWords -> Table
updateTableWithNormalEmail (T t spam normal) = 
    (\newTable -> T newTable spam $ normal + 1) . 
        updateTableHelp (\(W iD str mSpam mNormal) -> W iD str mSpam $ mNormal + 1) t

updateTableWithSpamEmail :: Table -> EndWords -> Table
updateTableWithSpamEmail (T t spam normal) = 
    (\newTable -> T newTable (spam + 1) normal) . 
        updateTableHelp (\(W iD str mSpam mNormal) -> W iD str (mSpam + 1) mNormal) t