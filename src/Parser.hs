{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser(main, parseJLFile) where
    import qualified Control.Applicative as CA(empty)
    import qualified Data.Aeson as JSON(decode, parseJSON , object, FromJSON, Value(Object), (.:))
    import qualified Data.ByteString as DBS(fromStrict, hGetLine, ByteString)
    import qualified Data.List as DL(drop, head, intersperse, isSuffixOf, tail, words)
    import qualified Data.Time as DT(UTCTime, getCurrentTime)
    import qualified GHC.Generics as GHCG(Generic)
    import qualified Network.URI as NW(parseURI, URI(uriPath, uriAuthority), URIAuth(uriRegName))
    import qualified System.IO as IO(hIsEOF, hGetLine, openFile, putStrLn, writeFile, Handle, IOMode(ReadMode))
    import qualified Text.HTML.TagSoup as TS(innerText, parseTags)
    import qualified Utils as Utils(encodeFileName, indexOf, indexOfReverse, lPadNumber, putTimeDiffFormatted, recreateDir, removeSubString, subString, validateFile)
    
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
        startTime <- DT.getCurrentTime

        parseJLLine' fileHandle destLinksDir destWordsDir 0 startTime

    -- | Loops via lines of 'fileHandle' and per line calls parseJLineContent where pass parsed JSON object of that line
    parseJLLine' :: IO.Handle -> FilePath -> FilePath -> Int -> DT.UTCTime -> IO()
    parseJLLine' fileHandle destLinksDir destWordsDir processedLineNumber startTime = do 
        isFileEnd <- IO.hIsEOF fileHandle
        if isFileEnd then 
            putSection' "Parsing completed!"
        else do
            lineRaw <- DBS.hGetLine fileHandle
            parseJLineContent' (JSON.decode (DBS.fromStrict lineRaw) :: Maybe JLLine) destLinksDir destWordsDir lineNumber startTime
            parseJLLine' fileHandle destLinksDir destWordsDir lineNumber startTime

        where lineNumber = processedLineNumber + 1

    -- | In case that input parse content has data in valid format, calls `parseJLineHtmlContent`
    -- with dest paths defined as input dest dir path + MD5 hash of url from content data.
    parseJLineContent' :: Maybe JLLine -> String -> String -> Int -> DT.UTCTime -> IO()
    parseJLineContent' parseMaybe destLinksDir destWordsDir lineNumber startTime =
        case parseMaybe of
            Nothing -> do 
                Utils.putTimeDiffFormatted startTime
                putStrLn $ lineId ++ " - skipped, invalid JSON structure"
            Just parse -> do
                case cleanUrl' $ url parse of
                    Nothing -> do
                        Utils.putTimeDiffFormatted startTime
                        putStrLn $ lineId ++ " - skipped, contains invalid URL"
                    Just url -> do 
                        let fileName = Utils.encodeFileName url
                        parseJLineHtmlContent' (html_content parse) (destLinksDir ++ "/" ++ fileName) (destWordsDir ++ "/" ++ fileName)     
                        Utils.putTimeDiffFormatted startTime        
                        putStrLn $ lineId ++ " - " ++ url ++ " - parsing complete."

        where lineId = " Line " ++ (show lineNumber) ++ "."

    -- | Parse links and words from html content and store it into files defined in `destLinksDir` and `destWordsDir`
    parseJLineHtmlContent' :: String -> String -> String-> IO()
    parseJLineHtmlContent' html destLinksFile destWordsFile = do
        IO.writeFile destWordsFile (concat (DL.intersperse "\n" rawWordsString))

        where clanedBodyContent = removePairTags' (pickPairTagContent' html "<body" "</body>") tagsToRemove
              rawWordsString = DL.words . TS.innerText $ TS.parseTags clanedBodyContent
              tagsToRemove = [("<noscript", "</noscript>"), ("<script", "</script>"), ("<style", "</style>")]

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
    removePairTag' html startTag endTag = removePairTag'' html startTag endTag (length endTag) 0
    removePairTag'' :: String -> String -> String -> Int -> Int -> String
    removePairTag'' html startTag endTag endTagLen offset = do
        let offsetHtml = DL.drop offset html
        case Utils.indexOf offsetHtml startTag of
            Nothing -> html
            Just startIndex -> do
                let startOffsetHtml = DL.drop startIndex offsetHtml
                case Utils.indexOf startOffsetHtml endTag of
                    Nothing -> html
                    Just endIndex -> do
                        removePairTag'' (Utils.removeSubString html rStartIndex rEndIndex) startTag endTag endTagLen (offset + startIndex)
                        where rEndIndex = startIndex + offset + endIndex + endTagLen
                              rStartIndex = startIndex + offset

    -- | Removes all occurences of content between start and end of all tags defines inside 'tags' in format: [(startTag1, endTag1), (...)] from 'html'.
    removePairTags' :: String -> [(String , String)] -> String
    removePairTags' html [] = html
    removePairTags' html tags = removePairTags' (removePairTag' html (fst tag) (snd tag)) (DL.tail tags)
        where tag = DL.head tags

---------------------------------------------- TODO COLLECTION ----------------------------------------------

-- uniqArrEl' :: Eq a => [a] -> [a]
-- uniqArrEl' [] = []
-- uniqArrEl' (x:xs) = x : uniqArrEl' (DL.filter (/=x) xs)

------------- PARSER links
-- parse content of href from html, store into another location under file with same name
-- parse links from file

------------- PARSER ADVANCED part
-- remove dots, comas, etc from parsed text (probably replace for space, if next or prev char is no space)
-- lowercase parser words
-- order words alhabetical
-- remove stop words (probably add better dic for stopwords)
