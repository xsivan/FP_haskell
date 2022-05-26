module App where
    import System.IO
    import Data.List
    import Data.Char (isAlpha, toLower, isSpace)
    import qualified Data.Set
    import qualified Data.Map as Map 

    main :: IO()
    main = do
            tmp <- removeStopWords
            print (tmp)

    removeStopWords :: IO [String]
    removeStopWords = do  
                putStrLn "nacitaj file"   
                handle <- readFile "src/stopwords.txt"
                file <- getLine
                hndl <- readFile file
                return (toWords hndl (lines handle))

    toWords :: String -> [String] -> [String]
    toWords s list = filter (\w -> w `notElem` list) (words s)
