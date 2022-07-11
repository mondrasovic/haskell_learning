import Data.Char

encryptChar :: Int -> Char -> Char
encryptChar k c
    | isAlpha c = chr $ ((ord c - shift + k) `mod` 26) + shift
    | otherwise = c
    where shift = ord $ if isLower c then 'a' else 'A'

caesarCipher :: Int -> String -> String
caesarCipher k msg = map (encryptChar k) msg

main :: IO()
main = do
    getLine
    msg <- getLine
    kStr <- getLine
    let k = read kStr :: Int
    putStrLn $ caesarCipher k msg
