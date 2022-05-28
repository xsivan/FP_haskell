{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser(parseJLFile) where
    import qualified Control.Applicative as CA(empty)
    import qualified Data.Aeson as JSON(decode, parseJSON, (.:), FromJSON, Value(Object))
    import qualified Data.ByteString as DBS(fromStrict, hGetLine)
    import qualified Data.List as DL(drop, intersperse, isPrefixOf, isSuffixOf, words)
    import qualified Data.Maybe as DM(catMaybes)
    import qualified Data.String as DS(IsString)
    import qualified Data.Time as DT(diffUTCTime, getCurrentTime, UTCTime)
    import qualified System.IO as IO(hIsEOF, openFile, hPutStr, putStrLn, hSetEncoding, withFile, writeFile, utf8, Handle, IOMode(ReadMode, WriteMode))
    import qualified GHC.Generics as GHCG(Generic)
    import qualified Network.URI as NW(parseURI, URI(uriPath, uriAuthority), URIAuth(uriRegName))
    import qualified System.IO as IO(hIsEOF, openFile, putStrLn, Handle, IOMode(ReadMode))
    import qualified Text.HTML.TagSoup as TS(fromAttrib, innerText, isTagOpenName, parseTags, Tag(TagOpen))
    import qualified Utils as Utils(encodeFileName, indexOf, indexOfReverse, lPadNumber, recreateDir, subString, toLowerStringArr, uniqArr, validateFile, writeToFileUTF8)
    
    data JLLine = JLLine {html_content :: String, url :: String} deriving (GHCG.Generic, Show)

    instance JSON.FromJSON JLLine where
        parseJSON (JSON.Object v) = JLLine <$> v JSON..: "html_content" <*> v JSON..: "url"
        parseJSON _ = CA.empty

    -- | Removes all parts of url except its domain and sub-page and return it.
    cleanUrl' :: String -> Maybe String
    cleanUrl' urlValue =
        case NW.parseURI urlValue of
            Nothing -> Nothing
            Just uri -> case NW.uriAuthority uri of
                Nothing -> Nothing
                Just auth -> do
                    let parsedUrl = NW.uriRegName auth ++ NW.uriPath uri
                    if "/" `DL.isSuffixOf`parsedUrl then Just parsedUrl
                    else Just $ parsedUrl ++ "/"

    -- | Determinates if tag is <a> with href.
    isTagLinkWithHref' :: (Eq b, DS.IsString b) => TS.Tag b -> Bool
    isTagLinkWithHref' tag@(TS.TagOpen _ content) = "a" `TS.isTagOpenName` tag && not (null content) && ((=="href") . fst $ head content)
    isTagLinkWithHref' _ = False

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
                putTimeDiffFormatted' startTime
                putStrLn $ lineId ++ " - skipped, invalid JSON structure"
            Just parse -> do
                case cleanUrl' $ url parse of
                    Nothing -> do
                        putTimeDiffFormatted' startTime
                        putStrLn $ lineId ++ " - skipped, contains invalid URL"
                    Just urlVal -> do 
                        let fileName = Utils.encodeFileName urlVal
                        parseJLineHtmlContent' (html_content parse) (destLinksDir ++ "/" ++ fileName) (destWordsDir ++ "/" ++ fileName)     
                        putTimeDiffFormatted' startTime 
                        putStrLn $ lineId ++ " - " ++ urlVal ++ " - parsing complete."

        where lineId = " Line " ++ (show lineNumber) ++ "."

    -- | Parse links and words from html content and store it into files defined in `destLinksDir` and `destWordsDir`
    parseJLineHtmlContent' :: String -> String -> String-> IO()
    parseJLineHtmlContent' html destLinksFile destWordsFile = do
        Utils.writeToFileUTF8 destLinksFile (concat (DL.intersperse " " (Utils.uniqArr $  Utils.toLowerStringArr links)))
        Utils.writeToFileUTF8 destWordsFile (concat (DL.intersperse " " (Utils.uniqArr $  Utils.toLowerStringArr words)))
        
        where clanedBodyContent = removePairTags' (pickPairTagContent' html "<body" "</body>") tagsToRemove
              links = DM.catMaybes (map (\x -> (cleanUrl' x)) ((map (TS.fromAttrib ("href" :: String)).filter isTagLinkWithHref'.TS.parseTags) clanedBodyContent))
              tagsToRemove = ["script", "style","noscript"]
              words = DL.words . TS.innerText $ TS.parseTags clanedBodyContent

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

    -- | Puts diff of start time and actual time on standard output in format [mm:ss]
    putTimeDiffFormatted' :: DT.UTCTime -> IO ()
    putTimeDiffFormatted' startTime = do
        endTime <- DT.getCurrentTime
        let diffSecondsRaw = floor (DT.diffUTCTime endTime startTime) :: Int
        let diffMinutes = diffSecondsRaw `div` 60
        let diffSeconds = diffSecondsRaw - (diffMinutes * 60)

        putStr $ "[" ++ (Utils.lPadNumber (show diffMinutes) 2 '0') ++ ":" ++ Utils.lPadNumber (show diffSeconds) 2 '0' ++ "]"

    -- | Removes all content of 'tags' from 'html'.
    removePairTags' :: String -> [String] -> String
    removePairTags' html tags = removePairTags'' html tags Nothing
    removePairTags'' :: String -> [String] -> Maybe String -> String
    removePairTags'' [] _ _ = ""
    removePairTags'' (h:tml) tags foundStartTag =
        case foundStartTag of
            Nothing -> 
                if h == '<' 
                    then case removePairTagsMatchStart' tml tags of
                        Nothing -> [h] ++ removePairTags'' tml tags Nothing
                        Just startTag -> removePairTags'' (DL.drop (length $ startTag ++ ">") tml) tags (Just startTag)
                else [h] ++ removePairTags'' tml tags foundStartTag
            Just startTag -> do
                let endTag = "/" ++ startTag ++ ">"

                if and [h == '<', endTag `DL.isPrefixOf` tml] 
                    then removePairTags'' (DL.drop (length endTag) tml) tags Nothing
                else removePairTags'' tml tags foundStartTag

    -- | Returns first tag of `t:ags` which start `html` content.
    removePairTagsMatchStart' :: String -> [String] -> Maybe String
    removePairTagsMatchStart' _ [] = Nothing
    removePairTagsMatchStart' html (t:ags)
        | t `DL.isPrefixOf` html = Just t
        | otherwise = removePairTagsMatchStart' html ags
