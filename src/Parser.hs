module Parser(main, parseFiles) where
    import System.Directory
    import System.IO
    
    main :: IO ()
    main = return ()

    parseFile :: String -> String -> IO ()
    parseFile src dest = 
        putStrLn "Called parse file"
        -- Parse name from file
        -- Save into dest + fileName some rng words for now

    parseFiles :: String -> String -> IO ()
    parseFiles src dest = 
        putStrLn "Called parse files"
        -- Check if exist source location, if no exit with error message
        -- Check if exist target destination, if no create it
            -- if creation fails exit with error message
        -- Loop html files in source location, if there is no file exit with message
            -- Per file call parseFile func
            -- Try to implement progress-er into console, ideal with content update instead of append