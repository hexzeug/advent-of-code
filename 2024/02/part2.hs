import Data.Array (Ix(inRange))
import Data.List (inits, tails)
int :: String -> Int
int = read

parseReport :: String -> [Int]
parseReport = map int . words

oneShorterSublists :: [a] -> [[a]]
oneShorterSublists xs = zipWith (++) (init $ inits xs) (tail $ tails xs)

adjacents :: [a] -> [(a, a)]
adjacents = zip <*> tail

mapAdjacents :: (a -> a -> b) -> [a] -> [b]
mapAdjacents f = map (uncurry f) . adjacents

reportToDistances :: [Int] -> [Int]
reportToDistances = mapAdjacents (-)

reportSafe :: [Int] -> Bool
reportSafe xs = let ys = reportToDistances xs in all (inRange (1, 3)) ys || all (inRange (-3, -1)) ys

reportSafe' :: [Int] -> Bool
reportSafe' = any reportSafe . oneShorterSublists

main :: IO ()
main = do
    input <- readFile "2024/02/input.txt"
    print $ length . filter reportSafe' . map parseReport . lines $ input