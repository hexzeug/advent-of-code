int :: Char -> Int
int = read . (:"")

strip :: String -> String
strip = head . lines

expand :: [Int] -> [Int]
expand = concatMap exnd . zip [0..]
    where exnd (i, x) = replicate x (if even i then i `div` 2 else -1)

compact :: [Int] -> [Int]
compact [] = []
compact (x : xs)
    | x /= -1 = x : compact xs
    | null xs = []
    | last xs == -1 = compact (x : init xs)
    | otherwise = last xs : compact (init xs)

main :: IO ()
main = do
    input <- readFile "2024/09/input.txt"
    print $ sum . zipWith (*) [0..] . compact . expand . map int . strip $ input