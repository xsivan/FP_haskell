module Index where
    import System.IO( hClose, hGetContents, openFile, IOMode(ReadMode) )
    import System.IO.Unsafe ()
    import System.IO.Error ()
    import Data.List ( findIndex, isPrefixOf, sortBy, tails )
    import Data.Char ( toLower ) 
    import System.Directory ()
    import Control.Monad ( forM_ ) 
    import System.Environment ()
    import Data.List.Split ( splitOn )
    import qualified Utils

    
    findArguments :: Eq a => [a] -> [a] -> Maybe Int
    findArguments search str = (search `isPrefixOf`) `findIndex` (tails str)  

    findWord' :: Eq a => [a] -> [a] -> Bool
    findWord' search str = (search `isPrefixOf`) str

    findWord :: Eq a => [a] -> [a] -> Bool
    findWord terms text = do
            case findArguments terms text of
                    Just _ -> True
                    Nothing -> False


    writeFunction :: (Eq a1, Num a1, Show a2) => a1 -> [Char] -> a2 -> IO ()
    writeFunction id str pgr 
        | id == 1 = appendFile "src/inverted_index.txt" $ str ++ " ["
        | id == 2 = appendFile "src/inverted_index.txt" $ ("(" ++ str  ++ "," ++ show pgr ++ "),")
        | id == 3 = appendFile "src/inverted_index.txt" $ "(0,0)]" ++ str
        | otherwise = print("Done")


    getURL :: Foldable t => [Char] -> t (Int, b) -> IO ()
    getURL word indexes = do
            numberedFiles <- Utils.getListFiles "data/parse-words"
            forM_ indexes $  \index -> do
                    let number = fst index
                    let index = number - 1
                    let tmp = numberedFiles !! index
                    print (word ++ " --> " ++ Utils.decodeFileName (snd tmp))
    

    lowercase :: String -> String
    lowercase = map toLower 


    readWord :: IO [String]
    readWord = do
            putStrLn "hladaj pre slovo"
            word <- getLine
            let new_word = lowercase word
            return (words new_word)
                

    sortFloat :: Ord a1 => [(a2, a1)] -> [(a2, a1)]
    sortFloat xs = sortBy (\(_, a) (_, b) -> compare a b) xs

    main :: IO()
    main =  do 
            word <- readWord
            f <- openFile ("src/inverted_index.txt") ReadMode
            cnt <- hGetContents f
            let invertedContent = (unwords (lines cnt))
            let finded = findWord ((unwords word) ++ " [") invertedContent
            if finded then hClose f
            else do
                    writeFunction 1 (unwords word) 0
                    listOfFiles <- Utils.getListFiles "data/parse-words"
                    let pagerank = zip [1..100] [0.01,0.02..1] -- There will be pagerank function
                    forM_ listOfFiles $  \file -> do
                                    handle <- openFile ("data/parse-words/" ++ snd file) ReadMode
                                    contents <- hGetContents handle
                                    let page_rank = map (\pgr -> if (fst pgr == fst file) then (snd pgr) else 0) pagerank
                                    let t = map (\w -> findWord w (unwords (lines contents))) word
                                    let n = map (\bools -> if bools then 1 else 0) t
                                    if length word == sum n then writeFunction 2 (show (fst file)) (sum page_rank) else return ()

                    writeFunction 3 "\n" 0

            contents <- readFile "src/inverted_index.txt"
            let listInverted = lines contents
            forM_ listInverted $ \line -> do
                    let fileWords = words line
                    if fileWords /= [] then do
                            let fword = ((unwords word) ++ " [")
                            let f = findWord' fword (unwords fileWords)
                            if f then do 
                                    let id = length (splitOn " " (unwords word))
                                    let values = Utils.readingList (fileWords !! id)
                                    let removeTail = init values
                                    let sortedValues = sortFloat removeTail
                                    getURL (unwords word) sortedValues
                            else return ()
                    else return ()
               
        
        
        

    
