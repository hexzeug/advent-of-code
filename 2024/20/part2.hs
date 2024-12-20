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

distance :: (Int, Int) -> (Int, Int) -> Int
distance (i1, j1) (i2, j2) = abs (i1 - i2) + abs (j1 - j2)

neighbours :: (Int, Int) -> Int -> Int -> [Int]
neighbours size d x =
    let (i, j) = coords size x
    in map (idx size) . concatMap (filter (f (i, j)) . zip [(i - d)..(i + d)] . repeat) $ [(j - d)..(j + d)]
    where f x y = inBounds size y && distance x y <= d

cheats :: Array Int Int -> (Int, Int) -> Int -> [Int]
cheats track size x = map gain . filter ((0<) . (track !)) $ neighbours size 20 x
    where gain y = track ! y - track ! x - distance (coords size x) (coords size y)

main :: IO ()
main = do
    input <- readFile "2024/20/input.txt"
    let (size, road, track) = readTrack input
    print $ length . concatMap (filter (>=100) . cheats track size) $ road