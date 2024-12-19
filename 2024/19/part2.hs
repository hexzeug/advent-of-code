import Data.Char (isSpace)
import Data.Maybe (mapMaybe, listToMaybe, isJust)
import Data.List (stripPrefix)
import Data.Hashable (Hashable(hash))
import GHC.Arr (Array)
import qualified GHC.Arr as Arr (listArray, (!), (//))

type HashTable k v = Array Int [(k, v)]

split :: Char -> String -> [String]
split p str =
    let (x, xs) = break (==p) str
    in if null xs then [x] else x : split p (tail xs)

strip :: String -> String
strip = filter (not . isSpace)

readInput :: String -> ([String], [String])
readInput str =
    let (towels : _ : designs) = lines str
    in (map strip (split ',' towels), filter (not . null) designs)

arrangements :: (Hashable a) => HashTable [a] Int -> [[a]] -> [[a]] -> HashTable [a] Int
arrangements memo towels = foldl arrange memo
    where
        arrange memo desing
            | isJust (memo ! desing) = memo
            | null desing = memo // (desing, 1)
            | otherwise =
                let subdesigns = mapMaybe (`stripPrefix` desing) towels
                    memo' = arrangements memo towels subdesigns
                in memo' // (desing, sum . mapMaybe (memo' !) $ subdesigns)

tableSize :: Int
tableSize = 2 ^ 8

emptyTable :: (Hashable k) => HashTable k v
emptyTable = Arr.listArray (0, tableSize - 1) (replicate tableSize [])

tableIdx :: (Hashable k) => k -> Int
tableIdx = (`mod` tableSize) . hash

(!) :: (Hashable k) => HashTable k v -> k -> Maybe v
(!) table k = lookup k (table Arr.! tableIdx k)

(//) :: (Hashable k) => HashTable k v -> (k, v) -> HashTable k v
(//) table (k, v) = table Arr.// [(tableIdx k, (k, v) : table Arr.! tableIdx k)]

main :: IO ()
main = do
    input <- readFile "2024/19/input.txt"
    let (towels, desings) = readInput input
    let table = arrangements emptyTable towels desings
    print $ sum . mapMaybe (table !) $ desings