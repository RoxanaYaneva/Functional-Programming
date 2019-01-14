import System.IO

main = putStrLn "Hello, World"

io = do name <- getLine
        putStrLn $ "You entered: " ++ name

palindrome = do putStrLn "Please, enter a palidrome: "
                line <- getLine
                let revLine = reverse line
                if revLine == line then putStrLn "Thank you!"
                else do putStrLn $ line ++ " is not a palindorme"
                        palindrome

getInt :: IO Int
getInt = do line <- getLine
            return $ read line

findAverage :: IO Double
findAverage = do putStrLn "Please enter a number: "
                 n <- getInt
                 s <- readAndSum n
                 return $ (fromIntegral s) / (fromIntegral n)

readAndSum :: Int -> IO Int
readAndSum 0 = return 0
readAndSum n = do putStrLn "Please, enter a number: "
                  x <- getInt
                  s <- readAndSum $ n - 1
                  return $ x + s

average = do avg <- findAverage
             putStrLn $ "Average is: " ++ show avg

getInts n = sequence $ replicate n getInt

printRead s = do putStr $ s ++ " = " ; getInt
readCoordinates = mapM printRead ["x", "y", "z"]

readInt :: String -> IO Int
readInt s = do putStr $ "Please, enter " ++ s ++ ": "
               getInt

findAverage' = do n <- readInt "# of numbers"
                  l <- mapM (readInt . ("number #"++) . show) [1..n]
                  let s = sum l
                  return $ (fromIntegral s) / (fromIntegral n)

encrypt inFile outFile =
        do  h1 <- openFile inFile ReadMode
            text <- hGetContents h1
            h2 <- openFile outFile WriteMode
            hPutStr h2 text
