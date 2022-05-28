module App (initApp, searchText) where
    import qualified PageRank(computePageRank)
    import qualified Parser(parseJLFile)
    import qualified Index (iindex)
    
    -- | Parse file and initialize app for search. Format: initApp sourceJlFilePath parseDestDir. In case that parseDestDir exist it is cleaned.
    --
    -- Example: initApp "\/opt\/app\/data\/collection_100.jl" "\/opt\/app/data-parse"
    initApp :: FilePath -> FilePath -> IO()
    initApp srcFile parseDestDir = do 
        Parser.parseJLFile srcFile linksDest (parseDestDir ++ "/-words")
        PageRank.computePageRank linksDest
        where linksDest = (parseDestDir ++ "/-links")

    -- | Search text occurences on the parsed pages
    searchText :: IO ()
    searchText = Index.iindex
