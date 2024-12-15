import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

int :: String -> Integer
int = read . filter isDigit

isInteger :: Double -> Bool
isInteger x = x == fromIntegral (truncate x)

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = let (a, b) = break p xs in if null b then [a] else a : split p (tail b)

readLine :: String -> (Integer, Integer)
readLine str = let (x1 : x2 : _) = map int (split (==',') str) in (x1, x2)

mapTuple :: (t -> b) -> (t, t) -> (b, b)
mapTuple f (a, b) = (f a, f b)

readMachines :: [String] -> [((Integer, Integer), (Integer, Integer), (Integer, Integer))]
readMachines [] = []
readMachines (a : b : p : xs) = (readLine a, readLine b, mapTuple (+10000000000000) (readLine p)) : readMachines xs

(#) :: (Integer, Integer) -> (Integer, Integer) -> Integer
(#) (x1, x2) (y1, y2) = x1 * y2 - x2 * y1

intersection :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> Maybe (Integer, Integer)
intersection a b p =
    let n = fromIntegral (p # b) / fromIntegral (a # b)
        m = fromIntegral (p # a) / fromIntegral (b # a)
    in if isInteger n && isInteger m then Just (truncate n, truncate m) else Nothing

-- I:                   n * a1 + m * b1 = p1
-- II:                  n * a2 + m * b2 = p2
-- III: I - (b1/b2)*II: n * (a1 - (b1/b2) * a2) = p1 - (b1/b2) * p2
--                          <=> n = (p1 - b1 * p2 / b2) / (a1 - b1 * a2 / b2)
--                          <=> n = (p1 * b2 - p2 * b1) / (a1 * b2 - a2 * b1)
--                          <=> n = p # b / a # b
-- IV: I - (a1/a2)*II:  m = (p1 * a2 - p2 * a1) / (b1 * a2 - b2 * a1)
--                          <=> m = p # a / b # a

solve :: ((Integer, Integer), (Integer, Integer), (Integer, Integer)) -> Maybe (Integer, Integer)
solve (a, b, p) = intersection a b p

cost :: Num a => (a, a) -> a
cost (n, m) = n * 3 + m

main :: IO ()
main = do
    input <- readFile "2024/13/input.txt"
    print $ sum . map cost . mapMaybe solve . readMachines . filter (not . null) . lines $ input