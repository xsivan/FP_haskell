module Parser(main, parseFiles) where
    import qualified Data.ByteString as DBS(concat, drop, isPrefixOf, length, null, readFile, tail, take, writeFile, ByteString)
    import qualified Data.ByteString.Char8 as DBSC(pack, unpack)
    import qualified Data.Foldable as DF(length)
    import qualified Data.List as DL(filter, init, isSuffixOf, last, tail)
    import qualified Data.Maybe as DM (fromJust)
    import qualified GHC.Base as GB(fmap)
    import qualified System.Directory as SD(createDirectory, doesDirectoryExist, getDirectoryContents, setCurrentDirectory)
    import qualified System.IO as SIO(putStrLn)
    import qualified Text.HTML.TagSoup as TS((~/=), parseTags, innerText) -- TODO clean later
    
    -- | Defined only as cabal requirement, does nothing.
    -- main :: IO ()
    -- main = do return ()

    -- TODO replace for upper version later
    main :: IO ()
    main = do
        SD.setCurrentDirectory "/opt/app/data/pages"
        parseFile' "no_email_phi_30.oil.shopping_testing.0.html" "/opt/app/data/parse"

    -- | Loop via html files in 'src' location, extract words from them and store it into 'dest' location
    -- 
    -- In case that 'src' location is not valid, throw error. In case that 'dest' location doesnt exist, creates it.
    parseFiles :: FilePath -> FilePath -> IO ()
    parseFiles src dest = do
        validateDir' src "Source location doesnt exist!"
        createDir' dest
        files <-DL.filter isFileHtml' `GB.fmap` SD.getDirectoryContents src
        SD.setCurrentDirectory src
        putLineSeparator'
        parseFiles' files dest 0 (DF.length files)

    -- | Creates directory if dir in 'path' doesnt exist else do nothing.
    createDir' :: FilePath -> IO ()
    createDir' path = do
        exist <- SD.doesDirectoryExist path
        if not exist then SD.createDirectory path else return ()

    -- | Creates ByteString version of end html pair tag by 'tagName'.
    getPairTagEnd' :: String -> DBS.ByteString
    getPairTagEnd' tagName = DBSC.pack("</" ++ tagName ++ ">")
    
    -- | Creates ByteString version of start html pair tag by 'tagName'.
    getPairTagStart' :: String -> DBS.ByteString
    getPairTagStart' tagName = DBSC.pack("<" ++ tagName)

    -- | Find start index of 'needle' in 'haystick', returns 'Nothing' if there is no 'needle' in 'haystick'.
    indexOfDBS' :: DBS.ByteString -> DBS.ByteString -> Int -> Maybe Int
    indexOfDBS' haystick needle index
        | DBS.null haystick = Nothing
        | DBS.isPrefixOf needle haystick = Just index
        | otherwise = indexOfDBS' (DBS.tail haystick) needle (index + 1)

    -- | Determines if 'path' is to HTML file by its extension (it is simple 'String' suffix compare).
    isFileHtml' :: FilePath -> Bool
    isFileHtml' path = DL.isSuffixOf ".html" path

    -- | TODO
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

    -- | TODO
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

    -- | Put simle line separator to output.
    putLineSeparator' :: IO ()
    putLineSeparator' = putStrLn "----------------------------------------------------------------------------------------------------"

    -- | Removes all occurences of content between 'startTag' and 'endTag' from 'html'.
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

    -- | Removes content from 'startIndex' to 'endIndex' in 'text'.
    removeSubStrDBS' :: DBS.ByteString -> Int -> Int -> DBS.ByteString
    removeSubStrDBS' text startIndex endIndex = DBS.concat [subStrDBS' text 0 startIndex, subStrDBS' text endIndex (DBS.length text)]

    -- | Returns content from 'startIndex' to 'endIndex' in 'text'.
    subStrDBS' :: DBS.ByteString -> Int -> Int -> DBS.ByteString
    subStrDBS' text startIndex endIndex = DBS.take(endIndex - startIndex) (DBS.drop startIndex text)

    -- | Validates existence of directory via 'path'. If 'path' is directory do nothing, else throw error with specific 'errorMessage'.
    validateDir' :: FilePath -> String -> IO()
    validateDir' path errorMessage = do 
        exist <- SD.doesDirectoryExist path
        if not exist then error errorMessage else return ()

----------------------------------------------FOR GARBAGE COLLECT ----------------------------------------------

    -- uniqArrEl' :: Eq a => [a] -> [a]
    -- uniqArrEl' [] = []
    -- uniqArrEl' (x:xs) = x : uniqArrEl' (DL.filter (/=x) xs)