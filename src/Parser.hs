module Parser(main, parseFiles) where
    import System.Directory (createDirectory, doesFileExist, doesDirectoryExist, getDirectoryContents, setCurrentDirectory)
    import System.IO (putStrLn)
    
    main :: IO ()
    main = parseFiles "/opt/app/data/pages/" "/opt/app/data/parse"

    createDir' :: FilePath -> IO ()
    createDir' path = do
        exist <- doesDirectoryExist path
        if not exist then createDirectory path else return ()

    hasSuffix' :: String -> String -> Bool
    hasSuffix' [] [] = True
    hasSuffix' [] suffix = False
    hasSuffix' text [] = True
    hasSuffix' text suffix = if last text == last suffix then hasSuffix' (init text) (init suffix) else False

    isFileHtml' :: String -> Bool
    isFileHtml' path = hasSuffix' path ".html"

    parseFile' :: FilePath -> FilePath -> IO ()
    parseFile' fileName dest = do
        -- TODO fill file with real words
        writeFile (dest ++ "/" ++ fileName) "something"

    parseFiles :: FilePath -> FilePath -> IO ()
    parseFiles src dest = do
        validateDir' src "Source location doesnt exist!"
        createDir' dest
        files <-filter isFileHtml' `fmap` getDirectoryContents src
        setCurrentDirectory src
        parseFiles' files dest 0 (length files)

    parseFiles' :: [String] -> String -> Int -> Int -> IO()
    parseFiles' files dest iteration count = 
        if iteration >= count 
            then putStr ("\nParsing completed!\n\n")
        else do
            let iterationNumber = iteration + 1
            let fileName = head files
            putStr ("Parsing " ++ show iterationNumber ++ ". file of total " ++ show count ++ " - " ++ fileName ++ "\n")
            parseFile' fileName dest
            parseFiles' (tail files) dest iterationNumber count

    validateDir' :: FilePath -> String -> IO()
    validateDir' path errorMessage = do 
        exist <- doesDirectoryExist path
        if not exist then error errorMessage else return ()
