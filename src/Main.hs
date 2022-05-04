module Main where
import System.IO
import Data.List
import Data.Char (isAlpha, toLower, isSpace)
import qualified Data.Set
import qualified Data.Map as Map 
--import qualified Stem


toWords :: String -> [String] -> [String]
toWords s list = filter (\w -> w `notElem` list) (words s)


removeStopWords :: IO [String]
removeStopWords = do  
            putStrLn "nacitaj file"   
            handle <- readFile "app/stopwords.txt" 
            file <- getLine
            hndl <- readFile file
            return (toWords hndl (lines handle))


--removeStem :: [String] -> [String]
--removeStem xs = map Stem.stem xs

main :: IO()
main = do
        tmp <- removeStopWords
        print (tmp)

