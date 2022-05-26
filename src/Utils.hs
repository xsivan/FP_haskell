module Utils (decodeFileName, encodeFileName, indexOf, indexOfReverse, main, recreateDir, removeSubString, subString, validateFile, getListFiles, readingList) where
    import qualified Data.ByteString.Base64 as Base64(decodeLenient, encode)
    import qualified Data.ByteString.Char8 as DBSC(pack, unpack)
    import qualified Data.List as DL(drop, isPrefixOf, isSuffixOf, reverse, tail, take)
    import qualified System.Directory as SD(createDirectory, doesFileExist, doesDirectoryExist, removeDirectoryRecursive, listDirectory)

    -- | Decodes hashed file name.
    decodeFileName :: String -> String
    decodeFileName encodedFileName = DBSC.unpack (Base64.decodeLenient (DBSC.pack encodedFileName))

    -- | Hash filename.
    encodeFileName :: String -> String
    encodeFileName fileName = DBSC.unpack (Base64.encode (DBSC.pack fileName))

    -- | Find index for first occurence of 'needle' in 'haystick', returns 'Nothing' if there is no 'needle' in 'haystick'.
    indexOf :: String -> String -> Maybe Int
    indexOf haystick needle = indexOf' haystick needle 0
    indexOf' :: String -> String -> Int -> Maybe Int
    indexOf' haystick needle index
        | null haystick = Nothing
        | needle `DL.isPrefixOf` haystick = Just index
        | otherwise = indexOf' (DL.tail haystick) needle (index + 1)

    -- | Find index for last occurence of 'needle' in 'haystick', returns 'Nothing' if there is no 'needle' in 'haystick'.
    indexOfReverse :: String -> String -> Maybe Int
    indexOfReverse haystick needle = 
        case indexOf' (DL.reverse haystick) (DL.reverse needle) 0 of
            Nothing -> Nothing
            Just indexReversed -> Just $ (length haystick) - indexReversed - (length needle)

    -- | Defined only as cabal requirement, does nothing.
    main :: IO ()
    main = do return ()

    -- | ReCreates directory, that means if dir in 'path' exist remove it include its content and then create new one else only creates it.
    -- recreateDir :: FilePath -> IO ()
    recreateDir :: FilePath -> IO ()
    recreateDir path = do
        exist <- SD.doesDirectoryExist path
        if exist then (>>) removeDir createDir else createDir

        where createDir = SD.createDirectory path  
              removeDir = SD.removeDirectoryRecursive path

    -- | Removes content from 'startIndex' to 'endIndex' in 'text'.
    removeSubString :: String -> Int -> Int -> String
    removeSubString text startIndex endIndex = concat [subString text 0 startIndex, subString text endIndex (length text)]

    -- | Returns content from 'startIndex' to 'endIndex' in 'text'.
    subString :: String -> Int -> Int -> String
    subString text startIndex endIndex = DL.take(endIndex - startIndex) (DL.drop startIndex text)

    -- | Validates existence of file via 'path' and its extension match.
    -- If 'path' or extension is not correct throw error with specific 'errorMessage', else do nothing.
    validateFile :: FilePath -> String -> String -> IO()
    validateFile path fileExtension errorMessage = do
        exist <- SD.doesFileExist path
        if and [exist, matchSuffix] then return () else error errorMessage
        where matchSuffix = DL.isSuffixOf fileExtension path
    
    readingList :: String -> [(Int, Int)]
    readingList = read

    getListFiles :: FilePath -> IO [(Int, String)]
    getListFiles direct = do
        files <- SD.listDirectory direct
        let numberedPages = addNumbers files
        return numberedPages

    addNumbers :: [String] -> [(Int, String)]
    addNumbers = zip [1 ..]