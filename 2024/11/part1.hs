int :: String -> Int
int = read

str :: Int -> String
str = show

stones :: Int -> String -> Int
stones c s
    | c <= 0 = 1
    | s == "0" = stones (c - 1) "1"
    | even (length s) =
        let (a, b') = splitAt (length s `div` 2) s
            b = dropWhile (=='0') b'
        in stones (c - 1) a + stones (c - 1) (if not (null b) then b else "0")
    | otherwise = stones (c - 1) (str (int s * 2024))

main :: IO ()
main = do
    input <- readFile "2024/11/input.txt"
    print $ sum . map (stones 25) . words $ input