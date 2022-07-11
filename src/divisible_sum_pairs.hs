getPairs :: [Int] -> [(Int, Int)]
getPairs xs = [(xs !! i, xs !! j) | i <- [0..length xs - 1], j <- [(i + 1)..length xs - 1]]

countDivPairsNum :: [Int] -> Int -> Int
countDivPairsNum xs k = length $ filter divChecker $ getPairs xs
    where divChecker pair = (fst pair + snd pair) `mod` k == 0

main :: IO()
main = do
    inputSizeStr <- getLine
    valsStr <- getLine
    let k = read ((words inputSizeStr) !! 1) :: Int
    let vals = map (read :: String -> Int) $ words valsStr
    print $ countDivPairsNum vals k
