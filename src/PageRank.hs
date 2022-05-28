module PageRank(main) where
    import System.IO
    import System.Directory
    import           Data.Map    (Map, empty, insert, insertWith, lookup,
                              mapWithKey, member, size, toList)
    import           Data.Maybe  (fromJust)
    import           Debug.Trace (trace)
    import           Prelude     hiding (lookup)
    import           Text.Printf (printf)
    import qualified Utils as Utils(decodeFileName, encodeFileName)

    type Node = Int
    type PRValue = Double
    type PageRank = Map Node PRValue
    type InboundEdges = Map Node [Node]
    type OutboundEdges = InboundEdges

    newPageRank :: Int -> PageRank
    newPageRank n =
        let v :: Double; v = 1 / (fromIntegral n)
            in go n v empty
        where
            go :: Int -> Double -> PageRank -> PageRank
            go 0 _ pr = pr

            go n v pr =
                go (n-1) v $ insert (n-1) v pr

    postProcess :: (InboundEdges, OutboundEdges, Node) -> (InboundEdges, OutboundEdges)
    postProcess (iEdges, oEdges, maxNode) =
        let numNodes = maxNode + 1
            newIEdges = addAllNodes (numNodes-1) iEdges
            in loop (numNodes-1) newIEdges oEdges

        where
            loop :: Int -> InboundEdges -> OutboundEdges -> (InboundEdges, OutboundEdges)
            loop n iEdges oEdges
                | n < 0 = (iEdges, oEdges)
                | otherwise =
                    if member n oEdges then
                        loop (n-1) iEdges oEdges
                    else
                        let numNodes = maxNode + 1
                            newOEdges = insert n (filter (/= n) [0..maxNode]) oEdges
                            newIEdges = mapWithKey (\k v -> if k /= n then v ++ [n] else v) iEdges
                            in loop (n-1) newIEdges newOEdges
            addAllNodes :: Int -> InboundEdges -> InboundEdges
            addAllNodes n iEdges
                | n < 0 = iEdges
                | otherwise =
                    addAllNodes (n-1) $ insertWith (\new old -> new ++ old) n [] iEdges

    pG :: [(Int, Int)] -> (InboundEdges, OutboundEdges, PageRank)
    pG arr =
        let ls = arr
            (iEdges, oEdges) = postProcess $ foldl pL (empty, empty, 0) ls
            numNodes = size iEdges
            in (iEdges, oEdges, newPageRank numNodes)

    pL :: (InboundEdges, OutboundEdges, Node) -> (Int, Int) -> (InboundEdges, OutboundEdges, Node)
    pL (iEdges, oEdges, maxNode) docIdcomb =
        let (from, to) = (fst docIdcomb, snd docIdcomb)
            in (insertWith plusNode to [from] iEdges,
                insertWith plusNode from [to] oEdges,
                max to (max maxNode from))
        where
            plusNode :: [Node] -> [Node] -> [Node]
            plusNode new_node old_node =
                new_node ++ old_node

    loopProcess :: Int -> Double -> InboundEdges -> OutboundEdges -> PageRank -> PageRank
    loopProcess 0 _ _ _ pageRank = pageRank
    loopProcess n dampingFactor iEdges oEdges pageRank =
        let newPageRank = loop' ((size pageRank) - 1) empty
            in loopProcess (n-1) dampingFactor iEdges oEdges newPageRank
        where
            loop' :: Int -> PageRank -> PageRank
            loop' n pr
                | n < 0 = pr
                | otherwise =
                    let inbounds = fromJust $ lookup n iEdges
                        newPrValue = (+)
                            ((1 - dampingFactor) / (fromIntegral $ size iEdges))
                            (dampingFactor * (foldl calc 0 inbounds))
                        in loop' (n-1) $ insert n newPrValue pr
                    where
                        calc acc node =
                            let outbounds = fromJust $ lookup node oEdges
                                prValue = fromJust $ lookup node pageRank
                                in acc + prValue / (fromIntegral $ length outbounds)

    process :: [(Int, Int)] -> Int -> Double -> PageRank
    process input numIters dampingFactor =
        let (iEdges, oEdges, pageRank) = pG input
            in loopProcess numIters dampingFactor iEdges oEdges pageRank

---------------------------------------------------------------------------------------------
    
    mainDirectory :: String
    mainDirectory = "data/parse-links"

    testos :: [String] -> [String]
    testos [] = []
    testos (x:xs) =
        (Utils.decodeFileName x) : testos xs

    getListOfFiles :: FilePath -> IO [(Int, String)]
    getListOfFiles path = do 
        files <- listDirectory path
        let decoded = testos files
        let numberedPages = addDocId decoded
        return numberedPages
        
    addDocId :: [String] -> [(Int, String)]
    addDocId = zip [0..]

    getDocId :: [(Int, String)] -> String -> Int
    getDocId [] _ = -1
    getDocId (x:xs) pageName
        | snd x == pageName = fst x
        | otherwise         = getDocId xs pageName

    getFileWords :: String -> IO [String]
    getFileWords fileName = do  
        content <- readFile $ mainDirectory ++ "/" ++ (Utils.encodeFileName  fileName)
        return $ words content

    getFromToDocId :: [String] -> [(Int, String)] -> Int -> [(Int, Int)]
    getFromToDocId [] _ _ = []
    getFromToDocId (x:xs) polePages fromDocId =
        (fromDocId, getDocId polePages (x)) : getFromToDocId xs polePages fromDocId

    getElemOfArray :: Int -> [(Int, String)] -> (Int, String)
    getElemOfArray index inputList =
        inputList !! index

    pgr :: Int -> [(Int, String)] -> IO [(Int, Int)]
    pgr (-1) _ = return []
    pgr index listOfFiles = do
        let oneElement = getElemOfArray index listOfFiles
        pageNames <- getFileWords $ snd oneElement
        let fromDocId = getDocId listOfFiles (snd oneElement)
        let oneWebPgR = getFromToDocId pageNames listOfFiles fromDocId
        rest <- pgr (index - 1) listOfFiles
        return (oneWebPgR ++ rest)

    dropInvalidValues :: [(Int, Int)] -> [(Int, Int)]
    dropInvalidValues [] = []
    dropInvalidValues (x:xs)
        | fst x /= -1  && snd x /= -1 = x : dropInvalidValues xs
        | otherwise                   = dropInvalidValues xs

    main :: IO ()
    main = do
        putStrLn "Starting Page Ranking"
        let listOfFilesIO = getListOfFiles mainDirectory
        listOfFiles <- listOfFilesIO
        let outputPageRankData = pgr ((length listOfFiles) - 1) listOfFiles
        a <- outputPageRankData
        let b = dropInvalidValues a
        writeFile "/opt/app/src/pageRankData.txt" $ show $ toList $ process b 5 0.85
        putStrLn "Page ranking Ended"