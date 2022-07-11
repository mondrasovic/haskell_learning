splitGroups :: Int -> String -> [String]
splitGroups _ [] = []
splitGroups k s  = (take k s):(splitGroups k $ drop k s)

countChangedLetters :: String -> Int
countChangedLetters s = sum $ map count' $ splitGroups 3 s
    where count' chunk    = length $ filter checkChar $ zip "SOS" chunk
          checkChar (a,b) = a /= b

main :: IO()
main = do
    input <- getLine
    print $ countChangedLetters input
