{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser(main, parseJLFile) where
    import qualified Control.Applicative as CA(empty)
    import qualified Data.Aeson as JSON(decode, parseJSON, FromJSON, Value(Object), (.:))
    import qualified Data.ByteString as DBS(fromStrict, hGetLine)
    import qualified Data.List as DL(drop, intersperse, isPrefixOf, isSuffixOf, words)
    import qualified Data.Time as DT(UTCTime, getCurrentTime)
    import qualified GHC.Generics as GHCG(Generic)
    import qualified Network.URI as NW(parseURI, URI(uriPath, uriAuthority), URIAuth(uriRegName))
    import qualified System.IO as IO(hIsEOF, openFile, putStrLn, writeFile, Handle, IOMode(ReadMode))
    import qualified Text.HTML.TagSoup as TS(innerText, parseTags)
    import qualified Utils as Utils(encodeFileName, indexOf, indexOfReverse, putTimeDiffFormatted, recreateDir, subString, validateFile)
    
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
        parseJLFile "C:/Users/Admin/Visual Studio Code Projects/FP_haskell-main/data/collection_100.jl" "C:/Users/Admin/Visual Studio Code Projects/FP_haskell-main/data/parse-links" "C:/Users/Admin/Visual Studio Code Projects/FP_haskell-main/data/parse-words"

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
                    Just urlVal -> do 
                        let fileName = Utils.encodeFileName urlVal
                        parseJLineHtmlContent' (html_content parse) (destLinksDir ++ "/" ++ fileName) (destWordsDir ++ "/" ++ fileName)     
                        Utils.putTimeDiffFormatted startTime 
                        putStrLn $ lineId ++ " - " ++ urlVal ++ " - parsing complete."

        where lineId = " Line " ++ (show lineNumber) ++ "."

    -- | Parse links and words from html content and store it into files defined in `destLinksDir` and `destWordsDir`
    parseJLineHtmlContent' :: String -> String -> String-> IO()
    parseJLineHtmlContent' html destLinksFile destWordsFile = do
        IO.writeFile destWordsFile (concat (DL.intersperse "\n" rawWordsString))
        
        where clanedBodyContent = removePairTags' (pickPairTagContent' html "<body" "</body>") tagsToRemove
              rawWordsString = DL.words . TS.innerText $ TS.parseTags clanedBodyContent
              tagsToRemove = ["script", "style","noscript"]

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
