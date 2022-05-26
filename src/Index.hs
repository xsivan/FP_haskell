module Index where
    import System.IO
    import System.IO.Unsafe
    import System.IO.Error
    import Data.List
    import Data.Char (isAlpha, toLower, isSpace)
    import qualified Data.Set
    import qualified Data.Map as Map 
    import System.Directory
    import Control.Monad 
    import System.Environment
    import Data.List.Split
    import System.Random 


    readingList :: String -> [(Int, Int)]
    readingList = read

    getListFiles :: FilePath -> IO [(Int, String)]
    getListFiles direct = do 
                            files <- listDirectory direct
                            let numberedPages = addNumbers files
                            return numberedPages

    addNumbers :: [String] -> [(Int, String)]
    addNumbers = zip [1..]


    findArguments :: Eq a => [a] -> [a] -> Maybe Int
    findArguments search str = (search `isPrefixOf`) `findIndex` (tails str)  

    findWord' :: Eq a => [a] -> [a] -> Bool
    findWord' search str = (search `isPrefixOf`) str

    findWord :: Eq a => [a] -> [a] -> Bool
    findWord terms text = do
            case findArguments terms text of
                    Just value -> True
                    Nothing -> False


    writeIndex :: (Show a1, Show a2) => a1 -> a2 -> IO ()
    writeIndex number rnd = do
            appendFile "src/inverted_index.txt" $ ("(" ++ show number ++ "," ++ show rnd ++ "),")

    writeWord :: [Char] -> IO ()
    writeWord str = do
            appendFile "src/inverted_index.txt" $ str ++ " ["

    writeNewLine :: [Char] -> IO ()
    writeNewLine str = do
            appendFile "src/inverted_index.txt" $ "(0,0)]" ++ str


    generateRandom :: (Int,Int) -> Int
    generateRandom (a,b) = unsafePerformIO (getStdRandom (randomR (a,b)))


    getURL :: Foldable t => [Char] -> t (Int, b) -> IO ()
    getURL word indexes = do
            numberedFiles <- getListFiles "data/pages"
            forM_ indexes $  \index -> do
                    let number = fst index
                    let index = number - 1
                    let tmp = numberedFiles !! index
                    print (word ++ " --> " ++ snd tmp)
    

    lowercase :: String -> String
    lowercase = map toLower 


    readWord :: IO [String]
    readWord = do
            putStrLn "hladaj pre slovo"
            word <- getLine
            let new_word = lowercase word
            return (words new_word)
                

    main :: IO()
    main =  do 
            word <- readWord
            f <- openFile ("src/inverted_index.txt") ReadMode
            cnt <- hGetContents f
            let invertedContent = (unwords (lines cnt))
            let finded = findWord ((unwords word) ++ " [") invertedContent
            if finded then hClose f
            else do
                    writeWord (unwords word)
                    listOfFiles <- getListFiles "data/pages"
                    forM_ listOfFiles $  \file -> do
                                    handle <- openFile ("data/pages" ++ snd file) ReadMode
                                    contents <- hGetContents handle
                                    let rnd = generateRandom (1,100)
                                    let t = map (\w -> findWord w (unwords (lines contents))) word
                                    let n = map (\bools -> if bools then 1 else 0) t
                                    if length word == sum n then writeIndex (fst file) rnd else return()

                    writeNewLine "\n"

            contents <- readFile "src/inverted_index.txt"
            let listInverted = lines contents
            forM_ listInverted $ \line -> do
                    let fileWords = words line
                    if fileWords /= [] then do
                            let fword = ((unwords word) ++ " [")
                            let f = findWord' fword (unwords fileWords)
                            if f then do 
                                    let id = length (splitOn " " (unwords word))
                                    let values = readingList (fileWords !! id)
                                    let removeTail = init values
                                    let sortedValues = sortOn snd removeTail
                                    getURL (unwords word) sortedValues
                            else return ()
                    else return ()
               
        
        
        

    
