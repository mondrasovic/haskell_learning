parseInts :: String -> [Int]
parseInts s = map (read :: String -> Int) $ words s

isInRange :: Int -> Int -> Int -> Int
isInRange x a b = if (x >= a) && (x <= b) then 1 else 0

countInRange :: [Int] -> Int -> Int -> Int -> Int
countInRange xs origin a b = sum $ map (\x -> isInRange (x + origin) a b) xs

main :: IO()
main = do
    intervalSpec <- getLine
    treesSpec    <- getLine
    fruitSpec    <- getLine
    applesSpec   <- getLine
    orangesSpec  <- getLine
    let interval = parseInts intervalSpec
    let trees    = parseInts treesSpec
    let apples   = parseInts applesSpec
    let oranges  = parseInts orangesSpec
    let s        = interval !! 0
    let t        = interval !! 1
    print $ countInRange apples (trees !! 0) s t
    print $ countInRange oranges (trees !! 1) s t
