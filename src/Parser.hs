{-# LANGUAGE ViewPatterns #-}

module Parser(main, parseFiles) where
    import qualified Data.ByteString as DBS(breakSubstring, concat, drop, findIndex, isPrefixOf, length, null, readFile, tail, take, writeFile, ByteString)
    import qualified Data.ByteString.Char8 as DBSC(pack, unpack)
    import qualified Data.Foldable as DF(concat, length)
    import qualified Data.List as DL(drop, dropWhile, filter, init, intersperse, last, tail, take, words)
    import qualified Data.Maybe as DM (fromJust, fromMaybe)
    import qualified Data.Text as DT(last, pack, toLower, unpack)
    import qualified System.Directory as SD(createDirectory, doesDirectoryExist, getDirectoryContents, setCurrentDirectory)
    import qualified System.IO as SIO(putStr, putStrLn)
    import qualified System.Posix.Internals (puts)
    import qualified Text.HTML.TagSoup as TS((~/=), parseTags, innerText)
    
    main :: IO ()
    main = do
        SD.setCurrentDirectory "/opt/app/data/pages"
        parseFile' "no_email_phi_30.oil.shopping_testing.0.html" "/opt/app/data/parse"

    createDir' :: FilePath -> IO ()
    createDir' path = do
        exist <- SD.doesDirectoryExist path
        if not exist then SD.createDirectory path else return ()

    getPairTagEnd' :: String -> DBS.ByteString
    getPairTagEnd' tagName = DBSC.pack("</" ++ tagName ++ ">")
    
    getPairTagStart' :: String -> DBS.ByteString
    getPairTagStart' tagName = DBSC.pack("<" ++ tagName)

    -- TODO probably replace for inbuild func
    hasSuffix' :: String -> String -> Bool
    hasSuffix' [] [] = True
    hasSuffix' [] suffix = False
    hasSuffix' text [] = True
    hasSuffix' text suffix = if DL.last text == DL.last suffix then hasSuffix' (DL.init text) (DL.init suffix) else False

    -- TODO add documentation, example here
    -- |/O(n)/ Convert a lazy 'ByteString' into a strict 'ByteString'.
    --
    -- Note that this is an /expensive/ operation that forces the whole lazy
    -- ByteString into memory and then copies all the data. If possible, try to
    -- avoid converting back and forth between strict and lazy bytestrings.
    --
    indexOfDBS' :: DBS.ByteString -> DBS.ByteString -> Int -> Maybe Int
    indexOfDBS' haystick needle index
        | DBS.null haystick = Nothing
        | DBS.isPrefixOf needle haystick = Just index
        | otherwise = indexOfDBS' (DBS.tail haystick) needle (index + 1)

    isFileHtml' :: String -> Bool
    isFileHtml' path = hasSuffix' path ".html"

    parseFile' :: FilePath -> FilePath -> IO ()
    parseFile' fileName dest = do
        html <- DBS.readFile fileName
        DBS.writeFile (dest ++ "/" ++ fileName) (pickPairTag' html (getPairTagStart' "body") (getPairTagEnd' "body"))

        -- TODO to lower should be called on html
        -- TODO removing of par tags etc should be called somewhere here too or implemented arr input if there is multiple par tags

        -- let words = wordsInHtml $ TS.parseTags (DBSC.unpack (removePairTag' html "script"))
        -- writeFile (dest ++ "/" ++ fileName) (DF.concat (DL.intersperse " " words))
        -- where wordsInHtml = DL.words . TS.innerText . DL.dropWhile (TS.~/= "<body>") 
        -- let bodyContent = sort $ uniqArrEl' $ fromBody $ parseTags (unpack (toLower (pack fileContent)))
        -- writeFile (dest ++ "/" ++ fileName) (concat (intersperse " " bodyContent))
        -- where fromBody = words.innerText.dropWhile (~/= "<body>") 
        -- where fromFooter = unwords . drop 6 . words . innerText . take 2 . dropWhile (~/= "<li id=lastmod>")

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
    
    -- TODO optimize, search of endIndex should start after form startIndex, todo probably move into function to return start & end use for remot too
    pickPairTag' :: DBS.ByteString -> DBS.ByteString -> DBS.ByteString -> DBS.ByteString
    pickPairTag' html startTag endTag 
        | startIndex == Nothing = DBSC.pack ""
        | endIndex == Nothing = DBSC.pack ""
        | otherwise = subStrDBS' html (DM.fromJust startIndex) (DM.fromJust endIndex)

        where startIndex = indexOfDBS' html startTag 0
              endIndex = indexOfDBS' html endTag 0

    putLineSeparator' :: IO ()
    putLineSeparator' = putStrLn "----------------------------------------------------------------------------------------------------"

    removePairTag' :: DBS.ByteString -> DBS.ByteString -> DBS.ByteString -> DBS.ByteString
    removePairTag' html startTag endTag = removePairTag'' html startTag endTag (DBS.length endTag)

    -- TODO optimize, search of endIndex should start after form startIndex
    removePairTag'' :: DBS.ByteString -> DBS.ByteString -> DBS.ByteString -> Int -> DBS.ByteString
    removePairTag'' html startTag endTag endTagLen
        | startIndex == Nothing = html
        | endIndex == Nothing = html
        | otherwise = removePairTag'' (removeSubStrDBS' html (DM.fromJust startIndex) ((DM.fromJust endIndex) + endTagLen)) startTag endTag endTagLen
        
        where startIndex = indexOfDBS' html startTag 0
              endIndex = indexOfDBS' html endTag 0

    removeSubStrDBS' :: DBS.ByteString -> Int -> Int -> DBS.ByteString
    removeSubStrDBS' string startIndex endIndex = DBS.concat [subStrDBS' string 0 startIndex, subStrDBS' string endIndex (DBS.length string)]

    subStrDBS' :: DBS.ByteString -> Int -> Int -> DBS.ByteString
    subStrDBS' string startIndex endIndex = DBS.take(endIndex - startIndex) (DBS.drop startIndex string)

    uniqArrEl' :: Eq a => [a] -> [a]
    uniqArrEl' [] = []
    uniqArrEl' (x:xs) = x : uniqArrEl' (DL.filter (/=x) xs)

    validateDir' :: FilePath -> String -> IO()
    validateDir' path errorMessage = do 
        exist <- SD.doesDirectoryExist path
        if not exist then error errorMessage else return ()
