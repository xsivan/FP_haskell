module App (initApp, main, searchText) where
    import qualified Index(iindex)
    import qualified PageRank(computePageRank)
    import qualified Parser(parseJLFile)
    import qualified Utils(getParsePath, recreateDir, writeToFileUTF8, getParseInvertedIndexPath)
    
    -- | Just for cabal, does nothing
    main :: IO()
    main = return ()

    -- | Parse file and initialize app for search. Format: initApp sourceJlFilePath.
    --
    -- Example: initApp "//opt//app//data//collection_100.jl"
    initApp :: FilePath -> IO()
    initApp srcFile = do 
        Utils.recreateDir Utils.getParsePath
        Parser.parseJLFile srcFile
        PageRank.computePageRank
        Utils.writeToFileUTF8 Utils.getParseInvertedIndexPath ""

    -- | Search text occurences on the parsed pages
    searchText :: IO ()
    searchText = Index.iindex
