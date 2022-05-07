module Parser where
    import System.IO
    
    main :: IO ()
    main = return ()

    parseFile :: String -> String -> IO ()
    parseFile src dest = 
        putStrLn "Called parse file"

    parseFiles :: String -> String -> IO ()
    parseFiles src dest = 
        putStrLn "Called parse files"