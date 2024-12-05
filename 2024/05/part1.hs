int :: String -> Int
int = read

(|||) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
(|||) f g x = f x || g x

split :: (a -> Bool) -> [a] -> [[a]]
split p xs =
    let (x, xs') = break p xs
    in if not $ null xs'
        then x : split p (tail xs')
        else [xs]

subsequences2 :: [a] -> [[a]]
subsequences2 [a, b] = [[a, b]]
subsequences2 (a : b : xs) = [a, b] : subsequences2 (b : xs)

correct :: (Foldable t, Eq a) => t [a] -> [a] -> Bool
correct rules = all (`elem` rules) . subsequences2

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

main :: IO ()
main = do
    input <- readFile "2024/05/input.txt"
    let [rules, prints] = map (map $ map int . split ((=='|') ||| (==','))) . split null . lines $ input
    print $ sum . map middle . filter (correct rules) $ prints