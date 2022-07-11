main :: IO()
main = do
    getLine
    xsStr <- getLine
    let xs = [read w :: Int | w <- (words xsStr)] in do
        putStrLn $ show xs