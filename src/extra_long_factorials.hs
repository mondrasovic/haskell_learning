factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * (factorial $ n - 1)

main :: IO()
main = do
    nStr <- getLine
    let n = read nStr :: Integer
    print $ factorial n