import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

int :: String -> Int
int = read . filter isDigit

isInt :: Float -> Bool
isInt x = x == fromIntegral (truncate x)

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = let (a, b) = break p xs in if null b then [a] else a : split p (tail b)

readLine :: String -> (Int, Int)
readLine str = let (x1 : x2 : _) = map int (split (==',') str) in (x1, x2)

readMachines :: [String] -> [((Int, Int), (Int, Int), (Int, Int))]
readMachines [] = []
readMachines (a : b : p : xs) = (readLine a, readLine b, readLine p) : readMachines xs

(#) :: (Int, Int) -> (Int, Int) -> Int
(#) (x1, x2) (y1, y2) = x1 * y2 - x2 * y1

linearlyDependent :: (Int, Int) -> (Int, Int) -> Bool
linearlyDependent = error "never reached"
-- linearlyDependent a b = a # b == 0

intersection :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
intersection a b p =
    let n = fromIntegral (p # b) / fromIntegral (a # b)
        m = fromIntegral (p # a) / fromIntegral (b # a)
    in if isInt n && isInt m then Just (truncate n, truncate m) else Nothing

-- I:                   n * a1 + m * b1 = p1
-- II:                  n * a2 + m * b2 = p2
-- III: I - (b1/b2)*II: n * (a1 - (b1/b2) * a2) = p1 - (b1/b2) * p2
--                          <=> n = (p1 - b1 * p2 / b2) / (a1 - b1 * a2 / b2)
--                          <=> n = (p1 * b2 - p2 * b1) / (a1 * b2 - a2 * b1)
--                          <=> n = p # b / a # b
-- IV: I - (a1/a2)*II:  m = (p1 * a2 - p2 * a1) / (b1 * a2 - b2 * a1)
--                          <=> m = p # a / b # a

extendFromSecond :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
extendFromSecond a b p = error "never reached"
    -- let m = fst p `div` fst b
    --     mbp = fst p - (m * fst b)
    -- in if mbp `mod` fst a == 0 then Just (mbp `div` fst a, m) else Nothing

solve :: ((Int, Int), (Int, Int), (Int, Int)) -> Maybe (Int, Int)
solve (a, b, p) = intersection a b p
-- solve (a, b, p) = if linearlyDependent a b then extendFromSecond a b p else intersection a b p

cost :: Num a => (a, a) -> a
cost (n, m) = n * 3 + m

main :: IO ()
main = do
    input <- readFile "2024/13/input.txt"
    print $ sum . map cost . mapMaybe solve . readMachines . filter (not . null) . lines $ input