genPairSums :: [Int] -> [Int]
genPairSums xs = [xs !! i + xs !! j | i <- [0..length xs - 1], j <- [i + 1..length xs - 1]]

main :: IO()
main = do
    print "main"