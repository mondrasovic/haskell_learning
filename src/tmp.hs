import Data.Maybe
import Control.Monad
import Data.List

getMaybeVal :: Maybe Int -> Int
getMaybeVal x =
    case x of
        Nothing -> 0
        Just val -> val

isSubseq :: (Eq a) => [a] -> [a] -> Bool
isSubseq [] _     = True
isSubseq (a:as) b = if isNothing p then False else isSubseq as $ drop ((getMaybeVal p) + 1) b
    where p = findIndex (==a) b

containsHackerRank :: String -> String
containsHackerRank s = if isSubseq "hackerrank" s then "YES" else "NO"

main :: IO()
main = do
    nStr <- getLine
    inputs <- replicateM (read nStr) getLine
    putStrLn $ unlines $ map containsHackerRank inputs
