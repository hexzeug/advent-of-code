import qualified GHC.Arr as Arr (listArray, (!), (//), bounds)
import GHC.Arr (Array)
import qualified Data.Heap as Heap
import Data.Maybe (fromJust)
import Data.List (sort, group, elem)

type Vertex = ((Int, Int), Char)
type Graph = Array Int (Array Int Char)
type Vertecies a = Array Int (Array Int (a, a, a, a))

listArray :: [e] -> Array Int e
listArray xs = Arr.listArray (0, length xs - 1) xs

readGraph :: String -> Graph
readGraph = listArray . map listArray . lines

clockwise :: Vertex -> Vertex
clockwise (v, r)
    | r == 'E' = (v, 'S')
    | r == 'S' = (v, 'W')
    | r == 'W' = (v, 'N')
    | r == 'N' = (v, 'E')

counterclockwise :: Vertex -> Vertex
counterclockwise = clockwise . clockwise . clockwise

next :: Vertex -> Vertex
next ((i, j), r)
    | r == 'E' = ((i, j + 1), r)
    | r == 'S' = ((i + 1, j), r)
    | r == 'W' = ((i, j - 1), r)
    | r == 'N' = ((i - 1, j), r)

(@) :: Graph -> Vertex -> Char
(@) g ((i, j), _) = g Arr.! i Arr.! j

(!) :: Vertecies a -> Vertex -> a
(!) m ((i, j), r) = f (m Arr.! i Arr.! j)
    where
        f (a, b, c, d)
            | r == 'E' = a
            | r == 'S' = b
            | r == 'W' = c
            | r == 'N' = d

(//) :: Vertecies a -> (Vertex, a) -> Vertecies a
(//) m (((i, j), r), v) = m Arr.// [(i, (m Arr.! i) Arr.// [(j, f (m Arr.! i Arr.! j))])]
    where
        f (a, b, c, d)
            | r == 'E' = (v, b, c, d)
            | r == 'S' = (a, v, c, d)
            | r == 'W' = (a, b, v, d)
            | r == 'N' = (a, b, c, v)

bounds :: Array Int (Array Int a) -> (Int, Int)
bounds g = (snd (Arr.bounds g) + 1, snd (Arr.bounds (g Arr.! 0)) + 1)

edges :: Graph -> Vertex -> [(Int, Vertex)]
edges g v = (1000, clockwise v) : (1000, counterclockwise v) : [(1, next v) | (g @ next v) /= '#']

walkGraph :: Graph -> (Vertecies Int, Vertecies [Vertex])
walkGraph g =
    let (h, w) = bounds g
    in
        dijkstra
            g
            (
                listArray . replicate h . listArray . replicate w
                $ (maxBound :: Int, maxBound :: Int, maxBound :: Int, maxBound :: Int)
            )
            (
                listArray . replicate h . listArray . replicate w
                $ ([], [], [], [])
            )
            (Heap.singleton (0, (((-1, -1), '\NUL'), ((h - 2, 1), 'E'))))

dijkstra :: Graph -> Vertecies Int -> Vertecies [Vertex] -> Heap.MinPrioHeap Int (Vertex, Vertex) -> (Vertecies Int, Vertecies [Vertex])
dijkstra g ds ps q = case Heap.view q of
    Nothing -> (ds, ps)
    Just ((d, (p, v)), q') ->
        if ds ! v < d then dijkstra g ds ps q' else
        let ds' = ds // (v, d)
            ps' = ps // (v, p : (ps ! v))
        in dijkstra g ds' ps' $ foldl f q' (edges g v)
        where f q (w, u) = Heap.insert (d + w, (v, u)) q

paths :: Vertecies [Vertex] -> [Vertex] -> Vertex -> [Vertex]
paths ps path v
    | snd v == '\NUL' || v `elem` path = path
    | otherwise =
        foldl (paths ps) (v : path) (ps ! v)

paths1 :: Vertecies [Vertex] -> [Vertex] -> [Vertex]
paths1 ps = foldl (paths ps) []


unique :: Ord a => [a] -> [a]
unique = map head . group . sort

destinations :: Vertecies Int -> [Vertex]
destinations ds =
    let (_, w) = bounds ds
        vs = [((1, w - 2), 'E'), ((1, w - 2), 'N')]
        d = minimum . map (ds !) $ vs
    in filter ((==d) . (ds !)) vs

main :: IO ()
main = do
    input <- readFile "2024/16/input.txt"
    let (ds, ps) = walkGraph . readGraph $ input
    print $ length . unique . map fst . paths1 ps . destinations $ ds