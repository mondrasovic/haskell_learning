data Edge = Edge Int Int deriving (Show)

main :: IO()
main = do
    let e = Edge 10 20
    print e