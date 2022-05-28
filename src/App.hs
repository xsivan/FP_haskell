module App (searchText, initApp) where
    import qualified Parser(parseJLFile)
    import Index (iindex)
    
    -- | Parse file and initialize app for search. Format: initApp sourceJlFilePath parseLinksDestDir parseWordsDestDir
    --
    -- Example: initApp "\/opt\/app\/data\/collection_100.jl" "\/opt\/app/data\/parse-links" "\/opt\/app\/data\/parse-words"
    initApp :: FilePath -> FilePath -> FilePath -> IO ()
    initApp srcFile destLinksDir destWordsDir = Parser.parseJLFile srcFile destLinksDir destWordsDir
    -- TODO init other stuff as index

    -- | Search text occurences on the parsed pages
    searchText :: IO ()
    searchText = iindex

