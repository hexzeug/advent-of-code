import Data.Bits (Bits(shift, xor, (.&.)))
import Data.List (groupBy, sortOn)

int :: String -> Int
int = read

xorshift :: Int -> Int -> Int
xorshift n x = x `xor` x `shift` n .&. (2 ^ 24 - 1)

rand :: Int -> Int
rand = xorshift 11 . xorshift (-5) . xorshift 6

sequences :: [Int] -> [((Int, Int, Int, Int), Int)]
sequences (a : b : c : d : e : xs) = ((b - a, c - b, d - c, e - d), e) : sequences (b : c : d : e : xs)
sequences _ = []

groupOn :: Eq a => (t -> a) -> [t] -> [[t]]
groupOn f = groupBy (\a b -> f a == f b)

group :: Ord a => [(a, b)] -> [[(a, b)]]
group = groupOn fst . sortOn fst

sumSnds :: [(a, Int)] -> (a, Int)
sumSnds xs = (fst . head $ xs, sum . map snd $ xs)

main :: IO ()
main = do
    input <- readFile "2024/22/input.txt"
    print $ maximum . map (snd . sumSnds) . group . concatMap (map head . group . sequences . take 2000 . map (`mod` 10) . iterate rand . int) . lines $ input