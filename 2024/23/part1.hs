import Data.Char (ord, chr)
import GHC.Arr (accumArray, Array, (!))
import Data.List (sort, group)

num :: Char -> Int
num = subtract (ord 'a') . ord

name :: Int -> Char
name = chr . (+ ord 'a')

readEdges :: String -> [(Int, Int)]
readEdges = concatMap readEdge . lines

readEdge :: String -> [(Int, Int)]
readEdge [a1, a2, _, b1, b2] =
    let a = num a1 * 26 + num a2
        b = num b1 * 26 + num b2
    in [(a, b), (b, a)]

buildG :: [(Int, Int)] -> Array Int [Int]
buildG = accumArray (flip (:)) [] (0, 26 * 26 - 1)

hasPrefixT :: Int -> Bool
hasPrefixT = (== num 't') . (`div` 26)

pairs :: [a] -> [[a]]
pairs [_] = []
pairs (x : xs) = map (:[x]) xs ++ pairs xs

uniq :: Ord a => [a] -> [a]
uniq = map head . group . sort

triangles :: Array Int [Int] -> [[Int]]
triangles g = uniq . concatMap from $ [(num 't' * 26)..(num 't' * 26 + 25)]
    where
        from v
            | length (g ! v) < 2 = []
            | otherwise = map (sort . (v:)) . filter edge . pairs $ (g ! v)
        edge [u, v] = u `elem` (g ! v)

main :: IO ()
main = do
    input <- readFile "2024/23/input.txt"
    print $ length . triangles . buildG . readEdges $ input