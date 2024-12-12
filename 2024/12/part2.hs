import GHC.Arr (Array (Array), listArray, (//), bounds, (!), elems, array, assocs, indices, Ix)
import Data.List (zip5, find, sort, group, groupBy, sortBy)
import Data.Tuple (swap)

toArray :: [a] -> Array Int a
toArray xs = listArray (0, length xs - 1) xs

repeatArray :: (Int, Int) -> e -> Array Int e
repeatArray bounds x = listArray bounds (repeat x)

readFarm :: String -> ((Int, Int), Array Int Char)
readFarm str =
    let lns = lines str
    in ((length (head lns), length lns), toArray . concat $ lns)

neighbours :: (Int, Int) -> Int -> [Int]
neighbours (w, h) x =
    let i = x `div` w
        j = x `mod` w
    in map idx . filter inBounds $ [(i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1)]
    where
        inBounds (i, j) = 0 <= i && i < h && 0 <= j && j < w
        idx (i, j) = i * w + j

floodRegion :: ((Int, Int), Array Int Char) -> Int -> Array Int Int -> Int -> Array Int Int
floodRegion (size, plots) r regions x
    | regions ! x == r = regions
    | otherwise =
        let name = plots ! x
        in foldl (floodRegion (size, plots) r) (regions // [(x, r)]) . filter ((==name) . (plots !)) . neighbours size $ x

toRegions :: ((Int, Int), Array Int Char) -> ((Int, Int), Array Int Int)
toRegions (size, plots) = (size, f 0 (repeatArray (bounds plots) (-1)) (size, plots))
    where
        f :: Int -> Array Int Int -> ((Int, Int), Array Int Char) -> Array Int Int
        f r regions farm =
            case find ((==(-1)) . snd) . assocs $ regions of
                Nothing -> regions
                Just (x, _) -> f (r + 1) (floodRegion farm r regions x) farm

topEdges :: (Int, Int) -> Array Int Int -> [Int] -> [Int]
topEdges (w, h) regions = filter f
    where
        f x =
            let i = x `div` w
                j = x `mod` w
            in i == 0 || regions ! x /= regions ! (x - w)

botEdges :: (Int, Int) -> Array Int Int -> [Int] -> [Int]
botEdges (w, h) regions = filter f
    where
        f x =
            let i = x `div` w
                j = x `mod` w
            in i == h - 1 || regions ! x /= regions ! (x + w)

lefEdges :: (Int, Int) -> Array Int Int -> [Int] -> [Int]
lefEdges (w, h) regions = sort . map t . filter f
    where
        f x =
            let i = x `div` w
                j = x `mod` w
            in j == 0 || regions ! x /= regions ! (x - 1)
        t x =
            let i = x `div` w
                j = x `mod` w
            in i + j * h

rigEdges :: (Int, Int) -> Array Int Int -> [Int] -> [Int]
rigEdges (w, h) regions = sort . map t . filter f
    where
        f x =
            let i = x `div` w
                j = x `mod` w
            in j == w - 1 || regions ! x /= regions ! (x + 1)
        t x =
            let i = x `div` w
                j = x `mod` w
            in i + j * h

countStreets :: [Int] -> Int
countStreets [_] = 1
countStreets (a : b : xs) = if a + 1 == b then countStreets (b : xs) else 1 + countStreets (b : xs)

countSides :: (Int, Int) -> Array Int Int -> [Int] -> Int
countSides size regions xs =
    countStreets (topEdges size regions xs)
    + countStreets (lefEdges size regions xs)
    + countStreets (botEdges size regions xs)
    + countStreets (rigEdges size regions xs)

main :: IO ()
main = do
    input <- readFile "2024/12/input.txt"
    let (size, regionLookup) = toRegions . readFarm $ input
    let regionList = map (map snd) . groupBy (\ (x, _) (x', _) -> x == x') . sort . map swap . assocs $ regionLookup
    print $ sum $ zipWith (*) (map length regionList) (map (countSides size regionLookup) regionList)