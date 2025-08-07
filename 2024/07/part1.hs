int :: String -> Int
int = read

parseEquation :: String -> (Int, [Int])
parseEquation eq =
    let xs = words eq
    in (int . init . head $ xs, map int . tail $ xs)

solves :: Int -> [Int] -> Bool
solves _ [] = False
solves rs [rs'] = rs == rs'
solves rs eq =
    let (a : b : eq') = eq
    in solves rs ((a + b) : eq') || solves rs ((a * b) : eq')

main :: IO ()
main = do
    input <- readFile "2024/07/input.txt"
    print $ sum . map fst . filter (uncurry solves) . map parseEquation . lines $ input