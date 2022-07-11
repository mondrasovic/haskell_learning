meetingExists :: Int -> Int -> Int -> Int -> String
meetingExists x1 v1 x2 v2
    | deltaV == 0                        = "NO"
    | (deltaV * deltaX < 0) && divisible = "YES"
    | otherwise                          = "NO"
        where deltaV = v2 - v1
              deltaX = x2 - x1
              divisible = (deltaX `mod` deltaV) == 0

main :: IO()
main = do
    valsStr <- getLine
    let vals = map (read :: String -> Int) $ words valsStr
    putStrLn $ meetingExists (vals !! 0) (vals !! 1) (vals !! 2) (vals !! 3)