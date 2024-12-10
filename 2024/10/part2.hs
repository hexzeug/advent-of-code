import GHC.Arr (listArray, Array, assocs, (!), Ix (inRange), bounds)
int :: Char -> Int
int = read . (:"")

toArray :: [e] -> Array Int e
toArray xs = listArray (0, length xs - 1) xs

readTopMap :: String -> Array Int (Array Int Int)
readTopMap = toArray . map (toArray . map int) . lines

trailheads :: Array Int (Array Int Int) -> [(Int, Int)]
trailheads = concatMap f . assocs
    where f (i, x) = map ((i,) . fst) . filter ((==0) . snd) . assocs $ x

rating :: Array Int (Array Int Int) -> Int -> (Int, Int) -> Int
rating topMap v (i, j)
    | not (inRange (bounds topMap) i) = 0
    | not (inRange (bounds (topMap ! i)) j) = 0
    | v /= topMap ! i ! j = 0
    | topMap ! i ! j == 9 = 1
    | otherwise =
        rating topMap (v + 1) (i, j + 1)
        + rating topMap (v + 1) (i + 1, j)
        + rating topMap (v + 1) (i, j - 1)
        + rating topMap (v + 1) (i - 1, j)

main :: IO ()
main = do
    input <- readFile "2024/10/input.txt"
    let topMap = readTopMap input
    print $ sum . map (rating topMap 0) . trailheads $ topMap