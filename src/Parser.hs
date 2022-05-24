{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser(main, parseJLFile) where
    import qualified Control.Applicative as CA(empty)
    import qualified Data.Aeson as JSON(decode, parseJSON , object, FromJSON, Value(Object), (.:))
    import qualified Data.ByteString as DBS(fromStrict, hGetLine, ByteString)
    import qualified Data.List as DL(drop, isSuffixOf)
    import qualified Data.Tuple as DT(fst, snd)
    import qualified GHC.Generics as GHCG(Generic)
    import qualified Network.URI as NW(parseURI, URI(uriPath, uriAuthority), URIAuth(uriRegName))
    import qualified System.IO as IO(hIsEOF, hGetLine, openFile, putStrLn, writeFile, Handle, IOMode(ReadMode))
    import qualified Utils as Utils(encodeFileName, indexOf, indexOfReverse, recreateDir, removeSubString, subString, validateFile)
    
    data JLLine = JLLine {html_content :: String, url :: String} deriving (GHCG.Generic, Show)

    instance JSON.FromJSON JLLine where
        parseJSON (JSON.Object v) = JLLine <$> v JSON..: "html_content" <*> v JSON..: "url"
        parseJSON _ = CA.empty

    -- | Defined only as cabal requirement, does nothing.
    main :: IO ()
    main = do return ()

    -- TODO remove later
    test :: IO ()
    test = do
        parseJLFile "/opt/app/data/collection_100.jl" "/opt/app/data/parse-links" "/opt/app/data/parse-words"

    -- | Removes all parts of url except its domain and sub-page and return it.
    cleanUrl' :: String -> Maybe String
    cleanUrl' url =
        case NW.parseURI url of
            Nothing -> Nothing
            Just uri -> case NW.uriAuthority uri of
                Nothing -> Nothing
                Just auth -> do
                    let parsedUrl = NW.uriRegName auth ++ NW.uriPath uri
                    if "/" `DL.isSuffixOf`parsedUrl then Just parsedUrl
                    else Just $ parsedUrl ++ "/"

    -- | Loops via lines of 'srcFile'.jl file, from each line extract words and links of html and links and store it into 'destWordsDir' and 'destLinksDir' location
    -- 
    -- In case that 'srcFile' file is not valid, throw error. Recreates 'destWordsDir' or 'destLinksDir' directories.
    parseJLFile :: FilePath -> FilePath -> FilePath -> IO ()
    parseJLFile srcFile destLinksDir destWordsDir = do
        Utils.validateFile srcFile "jl" "Source file doesnt exist!"
        Utils.recreateDir destLinksDir
        Utils.recreateDir destWordsDir

        fileHandle <- IO.openFile srcFile IO.ReadMode
        parseJLLine' fileHandle destLinksDir destWordsDir 0

    -- | Loops via lines of 'fileHandle' and per line calls parseJLineContent where pass parsed JSON object of that line
    parseJLLine' :: IO.Handle -> FilePath -> FilePath -> Int -> IO()
    parseJLLine' fileHandle destLinksDir destWordsDir processedLineNumber = do 
        isFileEnd <- IO.hIsEOF fileHandle
        if isFileEnd then 
            putSection' "Parsing completed!"
        else do
            lineRaw <- DBS.hGetLine fileHandle
            parseJLineContent' (JSON.decode (DBS.fromStrict lineRaw) :: Maybe JLLine) destLinksDir destWordsDir lineNumber
            parseJLLine' fileHandle destLinksDir destWordsDir lineNumber

        where lineNumber = processedLineNumber + 1
    
    -- | In case that input parse content has data in valid format, calls `parseJLineHtmlContent`
    -- with dest paths defined as input dest dir path + MD5 hash of url from content data.
    parseJLineContent' :: Maybe JLLine -> String -> String -> Int -> IO()
    parseJLineContent' parseMaybe destLinksDir destWordsDir lineNumber =
        case parseMaybe of
            Nothing -> print $ lineId ++ " - skipped, invalid JSON structure"
            Just parse -> do
                case cleanUrl' $ url parse of
                    Nothing -> print $ lineId ++ " - skipped, contains invalid URL"
                    Just url -> do 
                        let fileName = Utils.encodeFileName url
                        putStrLn $ lineId ++ " - " ++ url ++ " - starting parsing of html content."
                        parseJLineHtmlContent' (html_content parse) (destLinksDir ++ "/" ++ fileName) (destWordsDir ++ "/" ++ fileName)             

        where lineId = show lineNumber ++ ". line"

    -- | Parse links and words from html content and store it into files defined in `destLinksDir` and `destWordsDir`
    parseJLineHtmlContent' :: String -> String -> String-> IO()
    parseJLineHtmlContent' html destLinksFile destWordsFile = do
        let clanedBodycontent = pickPairTagContent' html "<body" "</body>"
        -- let bodyContent = pickPairTagContent' html "<body" "</body>"
        print destLinksFile

        where tagsToRemove = [("<noscript", "</noscript>"), ("<script", "</script>"), ("<style", "</style>")]

    -- | Picks content from first occurence of 'startTag' to first occurence of 'endTag'
    pickPairTagContent' :: String -> String -> String -> String
    pickPairTagContent' html startTag endTag =
        case Utils.indexOf html startTag of
            Nothing -> ""
            Just startIndex ->
                case Utils.indexOfReverse html endTag of
                    Nothing -> ""
                    Just endIndex -> Utils.subString html startIndex (endIndex + length endTag)

    -- | Put section formatted text to output.
    putSection' :: String -> IO()
    putSection' title = (>>)((>>) putSectionSeparator' (IO.putStrLn $ "--- " ++ title)) putSectionSeparator'

    -- | Put simle section separator to output.
    putSectionSeparator' :: IO ()
    putSectionSeparator' = IO.putStrLn "----------------------------------------------------------------------------------------------------"

    -- | Removes all occurences of content between 'startTag' and 'endTag' from 'html'.
    removePairTag' :: String -> String -> String -> String
    removePairTag' html startTag endTag = removePairTag'' html startTag endTag (length endTag)
    removePairTag'' :: String -> String -> String -> Int -> String
    removePairTag'' html startTag endTag endTagLen =
        -- TODO check optimization where you offset start index by last remove of same tag and end index by start index
        case Utils.indexOf html startTag of
            Nothing -> html
            Just startIndex ->
                case Utils.indexOf html endTag of
                    Nothing -> html
                    Just endIndex -> removePairTag'' (Utils.removeSubString html startIndex (endIndex + endTagLen)) startTag endTag endTagLen

    -- | Removes all occurences of content between start and end of all tags defines inside 'tags' in format: [(startTag1, endTag1), (...)] from 'html'.
    removePairTags' :: String -> [(String , String)] -> String
    removePairTags' html [] = html
    removePairTags' html tags = removePairTags' (removePairTag' html (fst tag) (snd tag)) (tail tags)
        where tag = head tags