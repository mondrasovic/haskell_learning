removeFirstDuplicate :: String -> String
removeFirstDuplicate []      = []
removeFirstDuplicate (a:[])  = [a]
removeFirstDuplicate (a:b:s) = if a == b then s else a:(removeFirstDuplicate $ b:s)

removeAllDuplicates :: String -> String
removeAllDuplicates s = if length s == length next then s else removeAllDuplicates next
    where next = removeFirstDuplicate s

reduceStr :: String -> String
reduceStr s = if length res == 0 then "Empty String" else res
    where res = removeAllDuplicates s

main :: IO()
main = do
    s <- getLine
    putStrLn $ reduceStr s
