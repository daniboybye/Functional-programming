module SpamFilter.FileFunctions
( loadTable
, saveTable
, createReport
, module SpamFilter.Table
, fileNameData
) where

import System.IO
import System.Directory

import Data.Function
import Data.Binary
import Data.List
import Data.Int
import Control.Monad
import qualified Data.Set as Set

import SpamFilter.Table
import SpamFilter.Algorithm

fileNameData :: String
fileNameData = "SpamFilter\\Data.bin" 

dictionaryTextFileToBasicBinaryFile :: String -> String -> IO ()
dictionaryTextFileToBasicBinaryFile textFileName binFileName =
    withFile textFileName ReadMode $
        join . ((encodeFile binFileName . (\table -> T table 0 0) . Set.fromList . 
            zipWith (\iD str -> W iD str 0 0) [minBound::WordID ..] . lines) <$!>) . hGetContents    

loadTable :: String -> IO Table
loadTable = decodeFile

saveTable :: String -> Table -> IO ()
saveTable = encodeFile

createReport :: FilePath -> Table -> IO ()
createReport fileName (T table spam normal) = writeFile fileName $
    unlines [(++) "processed spam emails : " $ show spam,
        (++) "processed normal emails : " $ show normal,
            unlines . map (\x -> concat $ intersperse "\t" ["in spam:",
                show $ getMatchesInSpam x, "in normal:", 
                    show $ getMatchesInNormal x, "word:", getWord x]) $ Set.toList table]

            
-- functions for training data ----------------------

training :: String -> FilePath -> IO ()
training dataFileName directoryName = do
    table <- loadTable dataFileName
    (spamEmails, normalEmails) <-
        uncurry (on (,) (map $ (++) directoryName)) . partition (isPrefixOf "spm") <$> listDirectory directoryName
    let tree   = fromTable table
        help f = 
            foldl' (\aTable fileName ->
                    withFile fileName ReadMode $ 
                        ((f <$> aTable) <*>) . ((uniq . matching tree) <$!>) . hGetContents)
    join $ saveTable dataFileName <$> (flip (help updateTableWithNormalEmail) normalEmails . 
        help updateTableWithSpamEmail (return table) $ spamEmails)

nameDir  = "SpamFilter\\tren\\lemm_stop\\part2\\"

fileNameDataAlternative :: String
fileNameDataAlternative = "SpamFilter\\DataSMS.bin" 

makeBase = dictionaryTextFileToBasicBinaryFile "SpamFilter\\dataset1.txt" fileNameData

firstTrain = training fileNameData nameDir    
        
trainWithAll str = mapM_ (training fileNameData) $ map (\x -> concat [str,x,"\\"]) ["1","2", "3","4","5","6","7","8","9","10"]

trainWithAllData :: IO ()
trainWithAllData = do
    trainWithAll "SpamFilter\\tren\\bare\\part"
    trainWithAll "SpamFilter\\tren\\lemm\\part"
    trainWithAll "SpamFilter\\tren\\bare\\part"

trainWithFileWithSmses :: IO ()
trainWithFileWithSmses = do
    table <- loadTable fileNameData
    let tree = fromTable table
    (spamSMS, normalSMS) <-
        (uncurry (on (,) (map $ uniq . matching tree)) . partition (isPrefixOf "spam") . lines) 
            <$> readFile "SpamFilter\\SMS.txt"
    saveTable "SpamFilter\\DataSMS.bin" $ foldl' updateTableWithNormalEmail 
        (foldl' updateTableWithSpamEmail table spamSMS) normalSMS

compressData :: IO ()
compressData = do
        help fileNameData 
        help fileNameDataAlternative
    where help str =
            join $ saveTable str <$> 
                (\(T table s n) -> 
                    T (Set.filter ( \(W _ _ spam normal) -> on (||) (>3) spam normal) table) s n)
                        <$> loadTable str

data NewWord  = NW Int16 AcString Int16 Int16 
data NewTable = NT (Set.Set NewWord) Int16 Int16

instance Binary NewWord where
    put (NW iD str spam normal) = do
        put iD
        put str
        put spam
        put normal
    get = do 
        liftM4 NW get get get get

instance Eq NewWord where  
    (NW x _ _ _) == (NW y _ _ _) = x == y

instance Ord NewWord where
    compare (NW x _ _ _) (NW y _ _ _) = compare x y

instance Binary NewTable where
    put (NT table spam normal) = do
        putList $ Set.toList table
        put spam
        put normal
    get = liftM3 NT (Set.fromList <$> get) get get

castTypesInBinFail :: IO ()
castTypesInBinFail =
    join $ encodeFile fileNameData <$>
        (\(T table spam normal) -> 
            NT (Set.fromList . zipWith (\iD (W _ str sp norm) -> NW iD str (fromIntegral sp) (fromIntegral norm)) [minBound :: Int16 .. ] $ Set.toList table)
                (fromIntegral spam) (fromIntegral normal)) <$> 
                    loadTable "SpamFilter\\Data(WordID32_IntStat64).bin"

testSpamFilterOverSMS :: IO ()
testSpamFilterOverSMS = do
    table <- loadTable fileNameData
    let tree = fromTable table
    ((spamSpam, spamNormal), (normalSpam, normalNormal)) <-
        (uncurry (on (,) (uncurry (on (,) length) . partition (isSpam table) . map (uniq . matching tree))) . partition (isPrefixOf "spam") . lines) 
            <$> readFile "SpamFilter\\SMS.txt"
    putStrLn $ "Spam   -> Spam   : " ++ show spamSpam
    putStrLn $ "Spam   -> Normal : " ++ show spamNormal
    putStrLn $ "Normal -> Spam   : " ++ show normalSpam 
    putStrLn $ "Normal -> Normal : " ++ show normalNormal  


 