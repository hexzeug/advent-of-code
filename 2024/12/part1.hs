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

edges :: ((Int, Int), Array Int Int) -> Int -> Int
edges (size, regions) x = 4 - (length . filter ((==(regions ! x)) . (regions !)) $ neighbours size x)

toPrices :: ((Int, Int), Array Int Int) -> [Int]
toPrices (size, regions) =
    zipWith (*)
        (map length . groupBy cmpFst . sort . map swap $ assocs regions)
        (map (sum . map snd) . groupBy cmpFst . sort $ zip (elems regions) (map (edges (size, regions)) (indices regions)))
    where
        cmpFst (x, _) (x', _) = x == x'

main :: IO ()
main = do
    input <- readFile "2024/12/input.txt"
    print $ sum . toPrices . toRegions . readFarm $ input