import Data.Array (Ix(inRange))
int :: String -> Int
int = read

parseReport :: String -> [Int]
parseReport = map int . words

adjacents :: [a] -> [(a, a)]
adjacents = zip <*> tail

mapAdjacents :: (a -> a -> b) -> [a] -> [b]
mapAdjacents f = map (uncurry f) . adjacents

reportToDistances :: [Int] -> [Int]
reportToDistances = mapAdjacents (-)

reportSafe :: [Int] -> Bool
reportSafe x = let d = reportToDistances x in all (inRange (1, 3)) d || all (inRange (-3, -1)) d

main :: IO ()
main = do
    input <- readFile "2024/02/input.txt"
    print $ length . filter reportSafe . map parseReport . lines $ input