module App (initApp, main, searchText) where
    import qualified Control.Monad as CM(forever)
    import qualified Index(iindex)
    import qualified PageRank(computePageRank)
    import qualified Parser(parseJLFile)
    import qualified Utils(getParsePath, recreateDir, writeToFileUTF8, getParseInvertedIndexPath)
    
    -- | Main function of app
    main :: IO()
    main = do 
        putStrLn "\nZadajte cestu k JL súboru na parsovanie: "
        path <- getLine
        initApp path

        CM.forever searchText

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
