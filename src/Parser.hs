module Parser(main, parseFiles) where
    import Data.Foldable as DF(length)
    import Data.List as DL(drop, filter, init, last, tail, take)
    import Data.Text as DT(last, pack, toLower, unpack)
    import System.Directory as SD(createDirectory, doesDirectoryExist, getDirectoryContents, setCurrentDirectory)
    import System.IO as SIO(putStr, putStrLn, readFile, writeFile)
    -- import Text.HTML.TagSoup ((~/=), parseTags, innerText)

    main :: IO ()
    main = do
        parseFiles "/opt/app/data/pages" "/opt/app/data/parse"

    createDir' :: FilePath -> IO ()
    createDir' path = do
        exist <- SD.doesDirectoryExist path
        if not exist then SD.createDirectory path else return ()

    hasSuffix' :: String -> String -> Bool
    hasSuffix' [] [] = True
    hasSuffix' [] suffix = False
    hasSuffix' text [] = True
    hasSuffix' text suffix = if DL.last text == DL.last suffix then hasSuffix' (DL.init text) (DL.init suffix) else False

    isFileHtml' :: String -> Bool
    isFileHtml' path = hasSuffix' path ".html"

    parseFile' :: FilePath -> FilePath -> IO ()
    parseFile' fileName dest = do
        fileContent <- SIO.readFile fileName
        SIO.writeFile (dest ++ "/" ++ fileName) fileContent

    parseFiles :: FilePath -> FilePath -> IO ()
    parseFiles src dest = do
        validateDir' src "Source location doesnt exist!"
        createDir' dest
        files <-DL.filter isFileHtml' `fmap` SD.getDirectoryContents src
        SD.setCurrentDirectory src
        putLineSeparator'
        parseFiles' files dest 0 (DF.length files)

    parseFiles' :: [String] -> String -> Int -> Int -> IO()
    parseFiles' files dest iteration count = 
        if iteration >= count then do
            putLineSeparator'
            SIO.putStrLn ("Parsing completed!")
            putLineSeparator'
        else do
            let iterationNumber = iteration + 1
            let fileName = head files
            SIO.putStrLn ("Parsing " ++ show iterationNumber ++ ". file of total " ++ show count ++ " - " ++ fileName)
            parseFile' fileName dest
            parseFiles' (DL.tail files) dest iterationNumber count
    
    putLineSeparator' :: IO ()
    putLineSeparator' = putStrLn "----------------------------------------------------------------------------------------------------"

    subString' :: Int -> Int -> [a] -> [a]
    subString' startIndex endIndex = DL.take (endIndex - startIndex + 1) . DL.drop startIndex

    uniqArrEl' :: Eq a => [a] -> [a]
    uniqArrEl' [] = []
    uniqArrEl' (x:xs) = x : uniqArrEl' (DL.filter (/=x) xs)

    validateDir' :: FilePath -> String -> IO()
    validateDir' path errorMessage = do 
        exist <- SD.doesDirectoryExist path
        if not exist then error errorMessage else return ()
