import Data.List (transpose, sort)

int :: String -> Int
int = read

extractColumns :: String -> [[Int]]
extractColumns = transpose . map (map int . words) . lines

tuple :: [a] -> (a, a)
tuple (a : b : _) = (a, b)

groupPairsBySize :: [[Int]] -> [(Int, Int)]
groupPairsBySize = map tuple . transpose . map sort

distanceOfPair :: Num a => (a, a) -> a
distanceOfPair (a, b) = abs $ a - b

pairsToDistance :: [(Int, Int)] -> [Int]
pairsToDistance = map distanceOfPair

main :: IO ()
main = do
    input <- readFile "2024/01/input.txt"
    print $ sum . pairsToDistance . groupPairsBySize . extractColumns $ input