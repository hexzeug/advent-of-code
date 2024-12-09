import Data.List (transpose)
int :: Char -> Int
int = read . (:"")

strip :: String -> String
strip = head . lines

blocks :: [Int] -> [(Int, Int)]
blocks xs = zip (zipWith (-) (scanl1 (+) xs) xs) xs

seperate :: [a] -> ([(Int, a)], [a])
seperate = f . transpose . pairs
    where
        pairs [a] = [[a, a]]
        pairs (a : b : xs) = [a, b] : pairs xs
        f [a, b] = (reverse (zip [0..] a), init b)

compact :: [(Int, Int)] -> [(Int, (Int, Int))] -> [(Int, (Int, Int))]
compact _ [] = []
compact free ((i, (x, l)) : files)
    | null free = (i, (x, l)) : files
    | fst (last free) > x = compact (init free) ((i, (x, l)) : files)
    | otherwise =
    let (free', free'') = break ((>=l) . snd) free
    in if null free'' then (i, (x, l)) : compact free files else
        let (x', l') = head free''
        in (i, (x', l)) : compact (free' ++ ((x' + l, l' - l) : tail free'')) files

checksum :: [(Int, (Int, Int))] -> Int
checksum = sum . concatMap cksm
    where cksm (i, (x, l)) = map (*i) [x..(x + l - 1)]

main :: IO ()
main = do
    input <- readFile "2024/09/input.txt"
    print $ checksum . (uncurry . flip $ compact) . seperate . blocks . map int . strip $ input