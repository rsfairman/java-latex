{-# LANGUAGE LambdaCase #-}

import System.Environment
import System.Directory
import Data.List
import Data.Either
import Text.ParserCombinators.Parsec
import Control.Monad

-- This import requires compiling with 'ghc -package ghc'.
import MonadUtils

------------------------------------------
-- Code related to the "label database"
------------------------------------------

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
  contents <- readFile fname
  return $ map readLabelEntry $ lines contents


-- Used when looking up labels. It determines a given string matches
-- the label field in a LabelEntry.
labelMatches :: LabelEntry -> String -> Bool
labelMatches (LabelEntry _ _ comp _ _ ) s = comp == s

-- To get the parts
labelSection :: LabelEntry -> String
labelSection (LabelEntry _ _ _ s _) = s

labelPage :: LabelEntry -> String
labelPage (LabelEntry _ _ _ _ s) = s


--------------------------------------
-- Parsing Java code to a simple AST
--------------------------------------

data ParsedJava = SLComment String | MLComment String | 
                    JavaCode String | WhiteSpace String

instance Show ParsedJava where
  show (SLComment s) = s
  show (MLComment s) = s
  show (JavaCode s) = s
  show (WhiteSpace s) = s

parseJava :: String -> Either ParseError [ParsedJava]
parseJava input = parse parseJavaInput "" input

parseJavaInput :: Parser [ParsedJava]
parseJavaInput = manyTill javaPart eof

javaPart :: Parser ParsedJava
javaPart = parseSLComment <|> parseMLComment <|> javaCode

parseSLComment :: Parser ParsedJava
parseSLComment = do
  try $ string "//"
  guts <- manyTill anyChar (try $ string "\n")
  return $ SLComment ("//" ++ guts ++ "\n")

parseMLComment :: Parser ParsedJava
parseMLComment = do
  try $ string "/*"
  guts <- manyTill anyChar (try $ string "*/")
  return $ MLComment ("/*" ++ guts ++ "*/")

javaCode :: Parser ParsedJava
javaCode = do
  x <- many1 javaBite
  return $ JavaCode $ concat x

javaBite :: Parser String
javaBite = stringCode <|> quoteCode <|> nonStringCode

stringCode :: Parser String
stringCode = do
  try $ char '"'
  x <- manyTill stringChar (string "\"")
  return ("\"" ++ concat x ++ "\"")

stringChar :: Parser String
stringChar = stringNonEscape <|> stringEscape

stringNonEscape :: Parser String
stringNonEscape = do
  x <- noneOf "\\\""
  return [x]

stringEscape :: Parser String
stringEscape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf'u"
  return [d,c]

quoteCode :: Parser String
quoteCode = do
  try $ char '\''
  x <- manyTill quoteChar (string "'")
  return ("'" ++ concat x ++ "'")

quoteChar :: Parser String
quoteChar = quoteNonEscape <|> quoteEscape

quoteNonEscape :: Parser String
quoteNonEscape = do
  x <- noneOf "\\'"
  return [x]

quoteEscape :: Parser String
quoteEscape = do
  d <- char '\\'
  c <- oneOf "\\\"0nrvtbf'u"
  return [d,c]

nonStringCode :: Parser String
nonStringCode = (many1 $ noneOf "/'\"") <|> try goodSlash

goodSlash :: Parser String
goodSlash = do
  x <- char '/'
  y <- noneOf "/*"
  return [x,y]


------------------------------------------------------------
-- Finding and replacing \ref and \pageref in Java comments
------------------------------------------------------------

parseAndReplace :: String -> [LabelEntry] -> Either ParseError String
parseAndReplace input defs = runParser replaceComment defs "" input

replaceComment :: GenParser Char [LabelEntry] String
replaceComment = do
    x <- many1 (notMacro <|> isMacro <|> falseMacro)
    return $ concat x

notMacro :: GenParser Char [LabelEntry] String
notMacro = many1 $ noneOf "\\"

isMacro :: GenParser Char [LabelEntry] String
isMacro = verbMacro <|> refMacro <|> pagerefMacro

verbMacro :: GenParser Char [LabelEntry] String
verbMacro = do
    try $ string "\\verb"
    x <- anyChar
    guts <- many $ noneOf [x]
    void $ char x
    return guts

refMacro :: GenParser Char [LabelEntry] String
refMacro = do
    try $ string "\\ref{"
    guts <- many $ noneOf ['}']
    void $ char '}'
    defs <- getState
    let xs = filter (\t -> labelMatches t guts) defs    
    if (null xs)
        then return "UNDEFINED"
        else return $ labelSection $ head xs

pagerefMacro :: GenParser Char [LabelEntry] String
pagerefMacro = do
    try $ string "\\pageref{"
    guts <- many $ noneOf ['}']
    void $ char '}'
    defs <- getState
    let xs = filter (\t -> labelMatches t guts) defs
    if (null xs)
        then return "UNDEFINED"
        else return $ labelPage $ head xs

falseMacro :: GenParser Char [LabelEntry] String
falseMacro = do
    void $ char '\\'
    return ['\\']


-----------------
-- The top-level
-----------------

output :: [LabelEntry] -> ParsedJava -> String
output defs (JavaCode s) = s
output defs (WhiteSpace s) = s
output defs (SLComment s) = replaceMacro defs s
output defs (MLComment s) = replaceMacro defs s

replaceMacro :: [LabelEntry] -> String -> String
replaceMacro defs s = do
    let x  = parseAndReplace s defs
    case x of
        Left err -> show(err)
        Right valid -> valid

argsValid :: [String] -> IO Bool
argsValid names = do
    if (null names) || (length names /= 2)
        then return False
        else doFilesExist names

doFilesExist :: [String] -> IO Bool
doFilesExist names = allM (\s -> doesFileExist s) names


main :: IO ()
main = do
    args <- getArgs
  
    argsValid args >>= \case
        False -> putStrLn "Give me the labels file and the Java file."
        True -> do
            macroDefs <- readDB (args !! 0)
            contents <- readFile (args !! 1)
            let rawParse = parseJava contents

            case rawParse of
              Left err -> putStrLn (show(err)) 
              Right valid -> mapM_ putStr $ map (output macroDefs) valid 


