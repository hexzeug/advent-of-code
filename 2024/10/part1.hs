import GHC.Arr (listArray, Array, assocs, (!), Ix (inRange), bounds)
import Data.List (sort, group)
int :: Char -> Int
int = read . (:"")

toArray :: [e] -> Array Int e
toArray xs = listArray (0, length xs - 1) xs

readTopMap :: String -> Array Int (Array Int Int)
readTopMap = toArray . map (toArray . map int) . lines

trailheads :: Array Int (Array Int Int) -> [(Int, Int)]
trailheads = concatMap f . assocs
    where f (i, x) = map ((i,) . fst) . filter ((==0) . snd) . assocs $ x

peaks :: Array Int (Array Int Int) -> Int -> (Int, Int) -> [(Int, Int)]
peaks topMap v (i, j)
    | not (inRange (bounds topMap) i) = []
    | not (inRange (bounds (topMap ! i)) j) = []
    | v /= topMap ! i ! j = []
    | topMap ! i ! j == 9 = [(i, j)]
    | otherwise =
        peaks topMap (v + 1) (i, j + 1)
        ++ peaks topMap (v + 1) (i + 1, j)
        ++ peaks topMap (v + 1) (i, j - 1)
        ++ peaks topMap (v + 1) (i - 1, j)

unq :: Ord a => [a] -> Int
unq = length . group . sort

main :: IO ()
main = do
    input <- readFile "2024/10/input.txt"
    let topMap = readTopMap input
    print $ sum . map (unq . peaks topMap 0) . trailheads $ topMap