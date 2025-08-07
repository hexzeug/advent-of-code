import Data.List (transpose)

int :: String -> Int
int = read

extractColumns :: String -> [[Int]]
extractColumns = transpose . map (map int . words) . lines

occurences :: Eq a => a -> [a] -> Int
occurences x = length . filter (==x)

similarityScore :: [Int] -> Int -> Int
similarityScore right x = occurences x right * x

similarityScores :: [[Int]] -> [Int]
similarityScores [left, right] = map (similarityScore right) left

main :: IO ()
main = do
    input <- readFile "2024/01/input.txt"
    print $ sum . similarityScores . extractColumns $ input