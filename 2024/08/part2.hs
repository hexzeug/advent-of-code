import Data.Complex (Complex((:+)), realPart, imagPart)
import Data.List (sort, group, nub, permutations, tails)
import GHC.Float (int2Double)

antennas :: String -> [(Complex Double, Char)]
antennas = concatMap (uncurry antennaLine) . zip (map (0.0 :+) [0.0..]) . lines
    where antennaLine x = filter ((/='.') . snd) . zip (map ((+ x) . (:+ 0.0)) [0.0..])

width :: String -> Double
width = int2Double . length . head . lines

height :: String -> Double
height = int2Double . length . lines

pairs :: [(Complex Double, Char)] -> [(Complex Double, Complex Double)]
pairs [_] = []
pairs (x : xs) = (map ((fst x,) . fst) . filter ((== snd x) . snd) $ xs) ++ pairs xs

antinodes :: String -> Complex Double -> Complex Double -> [Complex Double]
antinodes input a b = antinodes1 input a b ++ antinodes1 input b a

antinodes1 :: String -> Complex Double -> Complex Double -> [Complex Double]
antinodes1 input a b =
    if inBounds (width input) (height input) b
        then b : antinodes1 input b (2 * b - a)
        else []

inBounds :: (Ord a, Num a) => a -> a -> Complex a -> Bool
inBounds w h x =
    let real = realPart x
        imag = imagPart x
    in 0 <= real && real < w && 0 <= imag && imag < h

main :: IO ()
main = do
    input <- readFile "2024/08/input.txt"
    print $ length . nub . concatMap (uncurry (antinodes input)) . pairs . antennas $ input