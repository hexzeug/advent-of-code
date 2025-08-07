import qualified GHC.Arr as Arr ((!), (//), listArray, bounds)
import GHC.Arr (Array)

int :: String -> Int
int = read

str :: Int -> String
str = show

digits :: Int -> Int
digits x = if x < 10 then 1 else 1 + digits (x `div` 10)

half :: Int -> (Int, Int)
half x =
    let k = 10 ^ (digits x `div` 2)
    in (x `div` k, x `mod` k)

memoizeStones :: Int -> Array Int [((Int, Int), Int)] -> Int -> Array Int [((Int, Int), Int)]
memoizeStones c memo s
    | memo ? (s, c) = memo
    | c == 0 = memo // ((s, c), 1)
    | even (digits s) =
        let (a, b) = half s
            memo' = memoizeStones (c - 1) (memoizeStones (c - 1) memo a) b
        in memo' // ((s, c), memo' ! (a, c - 1) + memo' ! (b, c - 1))
    | otherwise =
        let s' = if s == 0 then 1 else s * 2024
            memo' = memoizeStones (c - 1) memo s'
        in memo' // ((s, c), memo' ! (s', c - 1))

idx :: Array Int e -> (Int, Int) -> Int
idx arr (s, c) = (s * (blinks + 1) + c) `mod` (1 + snd (Arr.bounds arr))

(?!) :: Array Int [((Int, Int), Int)] -> (Int, Int) -> Maybe Int
(?!) arr key = lookup key (arr Arr.! idx arr key)

(!) :: Array Int [((Int, Int), Int)] -> (Int, Int) -> Int
(!) arr key = case arr ?! key of
    Nothing -> error ("key " ++ show key ++ " not found")
    Just x -> x

(?) :: Array Int [((Int, Int), Int)] -> (Int, Int) -> Bool
(?) arr key = case arr ?! key of
    Nothing -> False
    Just _ -> True

(//) :: Array Int [((Int, Int), Int)] -> ((Int, Int), Int) -> Array Int [((Int, Int), Int)]
(//) arr (key, value) =
    let i = idx arr key
    in arr Arr.// [(i, (key, value) : (arr Arr.! i))]

emptyMemo :: Array Int [((Int, Int), Int)]
emptyMemo = Arr.listArray (0, 2 ^ 8 - 1) (repeat [])

blinks :: Int
blinks = 75

main :: IO ()
main = do
    input <- readFile "2024/11/input.txt"
    let stones = map int . words $ input
    let memo = foldl (memoizeStones blinks) emptyMemo stones
    print $ sum . map ((memo !) . (,blinks)) $ stones