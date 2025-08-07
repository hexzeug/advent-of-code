import Data.Char (isDigit)
import Data.Maybe (isNothing)
import GHC.Arr (Array, listArray)
import qualified GHC.Arr as Arr ((!), (//))
import qualified Data.Heap as Heap
import Data.List (sort)

split :: Char -> String -> [String]
split p str =
    let (x, xs) = break (==p) str
    in if null xs then [x] else x : split p (tail xs)

int :: String -> Int
int = read . filter isDigit

sortedFind :: Ord a => a -> [a] -> Maybe a
sortedFind _ [] = Nothing
sortedFind e (x : xs)
    | x < e = sortedFind e xs
    | x == e = Just x
    | otherwise = Nothing

readCoord :: String -> (Int, Int)
readCoord str = let (i : j : _) = split ',' str in (int i, int j)

passable :: [(Int, Int)] -> (Int, Int) -> Bool
passable corrupted (i, j) = 0 <= i && i < size && 0 <= j && j < size && isNothing (sortedFind (i, j) corrupted)

edges :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
edges corrupted (i, j) = filter (passable corrupted) [(i, j + 1), (i + 1, j), (i, j - 1), (i - 1, j)]

(!) :: Array Int e -> (Int, Int) -> e
(!) arr (i, j) = arr Arr.! (i * size + j)

(//) :: Array Int e -> ((Int, Int), e) -> Array Int e
(//) arr ((i, j), v) = arr Arr.// [(i * size + j, v)]

resetDs :: Array Int Int
resetDs = listArray (0, size * size - 1) (replicate (size * size) (maxBound :: Int))

resetQ :: Heap.MinPrioHeap Int (Int, Int)
resetQ = Heap.singleton (0, start)

dijkstra :: Array Int Int -> Heap.MinPrioHeap Int (Int, Int) -> [(Int, Int)] -> Array Int Int
dijkstra ds q corrupted = case Heap.view q of
    Nothing -> ds
    Just ((d, v), q') ->
        if ds ! v <= d then dijkstra ds q' corrupted else
        let ds' = ds // (v, d)
            q'' = foldl push q' (edges corrupted v)
        in dijkstra ds' q'' corrupted 
        where push q u = Heap.insert (d + 1, u) q

size :: Int
size = 71

start :: (Int, Int)
start = (0, 0)

destination :: (Int, Int)
destination = (70, 70)

time :: Int
time = 1024

main :: IO ()
main = do
    input <- readFile "2024/18/input.txt"
    print $ (! destination) . dijkstra resetDs resetQ . sort . take time . map readCoord . lines $ input