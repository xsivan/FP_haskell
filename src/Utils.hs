module Utils(
    decodeFileName, encodeFileName, getListFiles, getParsePath, getParseLinksPath, getParseWordsPath, indexOf, indexOfReverse, lPadNumber, 
    readingList, recreateDir, removeSubString, subString, toLowerStringArr, toLowerString, uniqArr, validateFile, writeToFileUTF8
) where
    import qualified Data.ByteString.Base64 as Base64(decodeLenient, encode)
    import qualified Data.ByteString.Char8 as DBSC(pack, unpack)
    import qualified Data.Char as DC(toLower)
    import qualified Data.List as DL(drop, filter, isPrefixOf, isSuffixOf, replicate, reverse, tail, take)
    import qualified System.Directory as SD(createDirectoryIfMissing, doesFileExist, doesDirectoryExist, listDirectory, removeDirectoryRecursive)
    import qualified System.IO as IO(hPutStr, hSetEncoding, openFile, utf8, IOMode(WriteMode))
    
    -- | Decodes hashed file name.
    decodeFileName :: String -> String
    decodeFileName encodedFileName = DBSC.unpack (Base64.decodeLenient (DBSC.pack encodedFileName))

    -- | Hash filename.
    encodeFileName :: String -> String
    encodeFileName fileName = DBSC.unpack (Base64.encode (DBSC.pack fileName))

    getListFiles :: FilePath -> IO [(Int, String)]
    getListFiles direct = do
        files <- SD.listDirectory direct
        return $ getListFilesAddNumbers' files
    getListFilesAddNumbers' :: [String] -> [(Int, String)]
    getListFilesAddNumbers' = zip [0..]

    -- | Returns parse path.
    getParsePath :: FilePath
    getParsePath = "./parse/"

    -- | Returns links parse path.
    getParseLinksPath :: FilePath
    getParseLinksPath = getParsePath ++ "links"

    -- | Returns words parse path.
    getParseWordsPath :: FilePath
    getParseWordsPath = getParsePath ++ "words"

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

    -- | LPad input string by requiredLen - len of string with padChar characters
    lPadNumber :: String -> Int -> Char -> String
    lPadNumber value requiredLen padChar = (\x -> DL.replicate (requiredLen - length x) padChar ++ x) value

    -- | ReCreates directory, that means if dir in 'path' exist remove it include its content and then create new one else only creates it.
    -- recreateDir :: FilePath -> IO ()
    recreateDir :: FilePath -> IO ()
    recreateDir path = do
        exist <- SD.doesDirectoryExist path
        if exist then (>>) removeDir createDir else createDir

        where createDir = SD.createDirectoryIfMissing True path  
              removeDir = SD.removeDirectoryRecursive path

    readingList :: String -> [(Int, Float)]
    readingList = read


    -- | Removes content from 'startIndex' to 'endIndex' in 'text'.
    removeSubString :: String -> Int -> Int -> String
    removeSubString text startIndex endIndex = concat [subString text 0 startIndex, subString text endIndex (length text)]

    -- | Transformes string array to lowercase
    toLowerStringArr :: [String] -> [String]
    toLowerStringArr [] = []
    toLowerStringArr (a:bc) = (toLowerString a) : toLowerStringArr bc

    -- | Transformes string to lowercase
    toLowerString :: String -> String
    toLowerString [] = []
    toLowerString (s:tring) = (DC.toLower s) : toLowerString tring

    -- | Returns content from 'startIndex' to 'endIndex' in 'text'.
    subString :: String -> Int -> Int -> String
    subString text startIndex endIndex = DL.take(endIndex - startIndex) (DL.drop startIndex text)

    -- | Removes duplicates from array
    uniqArr :: [String] -> [String]
    uniqArr [] = []
    uniqArr (x:xs) = x : uniqArr(DL.filter (/=x) xs)

    -- | Validates existence of file via 'path' and its extension match.
    -- If 'path' or extension is not correct throw error with specific 'errorMessage', else do nothing.
    validateFile :: FilePath -> String -> String -> IO()
    validateFile path fileExtension errorMessage = do
        exist <- SD.doesFileExist path
        if and [exist, matchSuffix] then return () else error errorMessage
        where matchSuffix = DL.isSuffixOf fileExtension path

    -- | Writes content in file with UTF-8 encoding    
    writeToFileUTF8 :: FilePath -> String -> IO()
    writeToFileUTF8 filePath content = do
        fileHandle <- IO.openFile filePath IO.WriteMode
        IO.hSetEncoding fileHandle IO.utf8
        IO.hPutStr fileHandle content