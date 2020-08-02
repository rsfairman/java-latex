{-# LANGUAGE LambdaCase #-}

{-
This is the pa.hs file on the website. This is the same thing, but with a few
more comments. 

The way this is set up, you run

getaux dbfile something.aux

to pull all of the \label values out of something.aux and add them to dbfile.
So the dbfile can hold \label values from many .aux files.

-}

import System.Environment
import System.Directory
import Data.List
import Control.Monad
import MonadUtils

data LabelEntry = LabelEntry {
    fileName :: String,
    labelType :: String,
    latexLabel :: String,
    section :: String,
    page :: String
} deriving (Eq)

instance Show LabelEntry where
    show (LabelEntry f1 f2 f3 f4 f5) = f1 ++ " " ++ f2 ++ " " ++ f3 ++ " " ++
        f4 ++ " " ++ f5

readLabelEntry :: String -> LabelEntry
readLabelEntry s = do
    let xs = words s
    LabelEntry {fileName = xs !! 0,
                labelType = xs !! 1,
                latexLabel = xs !! 2,
                section = xs !! 3,
                page = xs !! 4
               }


-- Given the name for a database file, read it in.
readDB :: String -> IO [LabelEntry]
readDB fname = do
    -- The entire file, type String.
    contents <- readFile fname
    return $ map readLabelEntry $ lines contents


-- Given an .aux file name, parse the file.
readAuxFile :: String -> IO [LabelEntry]
readAuxFile fname = do
    contents <- readFile fname

    -- Parse out the items we care about.
    let rawEntries = 
            -- Get rid of the blank items.
            map (filter (\s -> length s > 0)) $
            -- Each line of input becomes a list of strings, some of which
            -- are blank or empty.
            map parseItem $ 
            -- Drop the first 10 characters from each line (i.e., "\newlabel{")
            map (drop 10) $ 
            -- Only those lines that start with "\newlabel".
            filter (\s -> isPrefixOf "\\newlabel" s) $
            (lines contents)
    
    return $ map (rawToLabelDB fname "arbitrary") rawEntries

-- Breaks a line from an .aux file into the three pieces we care about. It is
-- assumed that we have stringXstringX, etc., where X is some combination of 
-- '{' and '}'. We want to split on any combination of these. This generates
-- lots of empty strings which need to be filtered out.
parseItem :: String -> [String]
parseItem "" = []
parseItem s = firstString : (parseItem rest) where
    firstString = takeWhile (\c -> (c /= '{') && (c/= '}')) s
    rest = drop (length firstString + 1) s
                
-- Convert already parsed data from an .aux file to a LabelEntry value.
rawToLabelDB :: String -> String -> [String] -> LabelEntry
rawToLabelDB fname labelType xs = 
    LabelEntry {
        fileName = fname,
        labelType = labelType,
        latexLabel = xs !! 0,
        section = xs !! 1,
        page = xs !! 2
    }


main = do
    args <- getArgs
    argsValid args >>= \case
        False -> putStrLn "Provide database file, then aux file."
        True -> do
            knownReferences <- readDB (args !! 0)
            newReferences <- readAuxFile (args !! 1)
            
            let combinedReferences = union newReferences knownReferences

            -- Careful here since Haskell's lazy IO can cause problems 
            -- reading/writing to the same file. One way to deal with this
            -- would be to write to a temporary file, then copy to the final 
            -- destination. That's safest since nothing is lost in a crash.
            -- Another way (done here) is to do something that requires that
            -- combinedReferences is complete.
            let totSize = length combinedReferences

            when (totSize > 0) $
              writeFile (args !! 0) (unlines $ map show combinedReferences)

            putStrLn $ "total number of labels: " ++ show totSize
      

  
argsValid :: [String] -> IO Bool
argsValid names = do
    if (null names) || (length names /= 2)
      then return False
      else doFilesExist names

doFilesExist :: [String] -> IO Bool
doFilesExist names = allM (\s -> doesFileExist s) names



