main :: IO()
main = do
    generalStr <- getLine
    valsStr <- getLine
    billStr <- getLine
    let k = read ((words generalStr) !! 1) :: Int
    let vals = map (read :: String -> Int) $ words valsStr
    let charged = read billStr :: Int
    let actual = (sum vals - (vals !! k)) `div` 2
    putStrLn $ if charged == actual then "Bon Appetit" else (show (charged - actual))