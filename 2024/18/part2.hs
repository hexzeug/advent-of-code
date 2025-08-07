import Data.Char (isDigit)
import GHC.Arr (Array, listArray)
import qualified GHC.Arr as Arr ((!), (//))
import Data.List (sort, inits, find)
import Data.Maybe (fromJust)

split :: Char -> String -> [String]
split p str =
    let (x, xs) = break (==p) str
    in if null xs then [x] else x : split p (tail xs)

int :: String -> Int
int = read . filter isDigit

readCoord :: String -> (Int, Int)
readCoord str = let (i : j : _) = split ',' str in (int i, int j)

passable :: [(Int, Int)] -> (Int, Int) -> Bool
passable corrupted (i, j) = 0 <= i && i < size && 0 <= j && j < size && (i, j) `notElem` corrupted

edges :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
edges corrupted (i, j) = filter (passable corrupted) [(i, j + 1), (i + 1, j), (i, j - 1), (i - 1, j)]

(!) :: Array Int e -> (Int, Int) -> e
(!) arr (i, j) = arr Arr.! (i * size + j)

(//) :: Array Int e -> ((Int, Int), e) -> Array Int e
(//) arr ((i, j), v) = arr Arr.// [(i * size + j, v)]

resetVs :: Array Int Bool
resetVs = listArray (0, size * size - 1) (replicate (size * size) False)

resetQ :: [(Int, Int)]
resetQ = [start]

bfs :: Array Int Bool -> [(Int, Int)] -> [(Int, Int)] -> Array Int Bool
bfs vs [] _ = vs
bfs vs (v : q) corrupted =
    if vs ! v then bfs vs q corrupted else
    let vs' = vs // (v, True)
        q' = q ++ edges corrupted v
    in bfs vs' q' corrupted

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
    print $ last . fromJust . find (not . (! destination) . bfs resetVs resetQ) . inits . map readCoord . lines $ input