module Main where

import SpamFilter.FileFunctions
import SpamFilter.Algorithm

import System.IO
import System.Directory
import Control.Monad
import Data.Tuple

action :: AhoCorasick -> Table -> IO Table
action tree = action1
    where action1 :: Table -> IO Table
          action1 table = do
            done <- isEOF
            if done
                then exit
                else do
                        input <- getLine
                        case input of   "report"                   -> report
                                        "check email"              -> check
                                        "update with spam"         -> updateWithSpam
                                        "update with normal email" -> updateWithNormal
                                        "exit"                     -> exit
                                        _                          -> invalidInput       
            where   exit :: IO Table
                    exit = return table

                    report :: IO Table
                    report = do
                        putStr "file name: "
                        hFlush stdout
                        join $ flip createReport table <$> getLine
                        action1 table

                    invalidInput :: IO Table
                    invalidInput = do
                        putStrLn "commands: report, check email, update with spam, update with normal email and exit\n"
                        action1 table

                    h :: (Table -> EndWords -> IO Table) -> IO Table
                    h f = do
                        putStr "file name: "
                        hFlush stdout
                        fileName <- getLine
                        flag     <- doesFileExist fileName
                        if flag
                            then join $ (<$>) action1 .
                                   join $ (f table . uniq . matching tree . validateТheТext) <$!> readFile fileName 
                            else (do
                                    putStrLn "File doesn't exist"
                                    h f)

                    updateWithSpam   :: IO Table
                    updateWithSpam   = h $ (return .) . updateTableWithSpamEmail

                    updateWithNormal :: IO Table
                    updateWithNormal = h $ (return .) . updateTableWithNormalEmail 

                    check :: IO Table
                    check = h (\t w -> do
                                putStrLn . (++) "is spam: " . show $ isSpam t w
                                return t)
                        
                    
main :: IO ()
main = join $ saveTable fileNameData <$> (join $ liftM2 action fromTable id <$> loadTable fileNameData)