import Data.List

rowToInts :: String -> [Int]
rowToInts s = map (read :: String -> Int) $ words s

isFactor :: Int -> [Int] -> Bool
isFactor n xs = all (\x -> x `mod` n == 0) xs

areFactors :: [Int] -> Int -> Bool
areFactors xs n = all (\x -> n `mod` x == 0) xs

isCommonFactor :: [Int] -> Int -> [Int] -> Bool
isCommonFactor xs n ys = (areFactors xs n) && (isFactor n ys)

countCommonFactors :: [Int] -> [Int] -> Int
countCommonFactors xs ys = length $ filter (\n -> isCommonFactor xs n ys) [start..end]
    where start = maximum xs
          end   = minimum ys

main :: IO()
main = do
    getLine
    xsRow <- getLine
    ysRow <- getLine
    let xs = rowToInts xsRow
    let ys = rowToInts ysRow
    print $ countCommonFactors xs ys
