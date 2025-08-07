import Data.Maybe (fromJust)
import Data.List (elemIndex, find)
import GHC.Arr (Array, listArray, (!), (//))

readTrack :: [Char] -> ((Int, Int), [Int], Array Int Int)
readTrack str =
    let lns = lines str
        w = length (head lns)
        h = length lns
        lst = concat lns
        s = fromJust (elemIndex 'S' lst)
        e = fromJust (elemIndex 'E' lst)
        (road, track) = followRoad (w, h) s . listArray (0, w * h - 1) . map roadType $ lst
    in ((w, h), road, track)
    where
        roadType x | x == '#' = -1 | x == 'S' = 1 | otherwise = 0
        followRoad size x track =
            case find ((==0) . (track !)) (neighbours size 1 x) of
                Nothing -> ([x], track)
                Just y ->
                    let (road, track') = followRoad size y (track // [(y, (track ! x) + 1)])
                    in (x : road, track')

coords :: (Int, Int) -> Int -> (Int, Int)
coords (w, h) x = (x `div` w, x `mod` w)

idx :: (Int, Int) -> (Int, Int) -> Int
idx (w, h) (i, j) = i * w + j

inBounds :: (Int, Int) -> (Int, Int) -> Bool
inBounds (w, h) (i, j) = 0 <= i && i < h && 0 <= j && j < w

neighbours :: (Int, Int) -> Int -> Int -> [Int]
neighbours size d x =
    let (i, j) = coords size x
    in map (idx size) . filter (inBounds size) $ [(i + d, j), (i - d, j), (i, j + d), (i, j - d)]

cheats :: Array Int Int -> (Int, Int) -> Int -> [Int]
cheats track size x = map (subtract (2 + track ! x)) . filter (0<) . map (track !) $ neighbours size 2 x

main :: IO ()
main = do
    input <- readFile "2024/20/input.txt"
    let (size, road, track) = readTrack input
    print $ length . concatMap (filter (>=100) . cheats track size) $ road