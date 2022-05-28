module App(main, initApp, searchText) where
    import qualified Parser(parseJLFile)

    -- | Parse file and initialize app for search. Format: initApp sourceJlFilePath parseLinksDestDir parseWordsDestDir
    --
    -- Example: initApp "\/opt\/app\/data\/collection_100.jl" "\/opt\/app/data\/parse-links" "\/opt\/app\/data\/parse-words"
    initApp :: FilePath -> FilePath -> FilePath -> IO ()
    initApp srcFile destLinksDir destWordsDir = Parser.parseJLFile srcFile destLinksDir destWordsDir
    -- TODO init other stuff as index

    -- | Defined only as cabal requirement, does nothing.
    main :: IO ()
    main = do return ()

    -- | Search text occurences on the parsed pages
    searchText :: FilePath -> FilePath -> FilePath -> IO ()
    searchText text parseLinksDir parseWordsDir = putStrLn ("Not implemented - " ++ text ++ " - "  ++ parseLinksDir  ++ " - " ++ parseWordsDir)
