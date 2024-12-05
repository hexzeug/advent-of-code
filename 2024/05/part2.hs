import GHC.Arr (accumArray, assocs)
import Data.Foldable (find)
import Data.Maybe (fromJust)

int :: String -> Int
int = read

(|||) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
(|||) f g x = f x || g x

(&&&) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
(&&&) f g x = f x && g x

split :: (a -> Bool) -> [a] -> [[a]]
split p xs =
    let (x, xs') = break p xs
    in if not $ null xs'
        then x : split p (tail xs')
        else [xs]

subsequences2 :: [a] -> [[a]]
subsequences2 [a, b] = [[a, b]]
subsequences2 (a : b : xs) = [a, b] : subsequences2 (b : xs)

correct :: (Foldable t, Eq a) => t [a] -> [a] -> Bool
correct rules = all (`elem` rules) . subsequences2

tuple2 :: [a] -> (a, a)
tuple2 [a, b] = (a, b)

onSnd :: (t -> b) -> (a, t) -> (a, b)
onSnd f (a, b) = (a, f b)

buildG :: (Int, Int) -> [(Int, Int)] -> [(Int, [Int])]
buildG bds = assocs . accumArray (flip (:)) [] bds

leaf :: [(a1, [a2])] -> (a1, [a2])
leaf = fromJust . find (null . snd)

reverseTopSort :: [(Int, [Int])] -> [Int]
reverseTopSort [] = []
reverseTopSort g =
    let (x, _) = leaf g
    in
        x : reverseTopSort (map (onSnd (filter (/=x))) . filter ((/=x) . fst) $ g)

reversePageOrder :: [[Int]] -> [Int]
reversePageOrder = reverseTopSort . buildG (11, 99) . map tuple2

filterRules :: (Foldable t, Eq b) => t b -> [[b]] -> [[b]]
filterRules pages = filter (((`elem` pages) . head) &&& ((`elem` pages) . last))

reorder :: Foldable t => [[Int]] -> t Int -> [Int]
reorder rules xs = reverse . filter (`elem` xs) . reversePageOrder $ filterRules xs rules

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

main :: IO ()
main = do
    input <- readFile "2024/05/input.txt"
    let [rules, prints] = map (map $ map int . split ((=='|') ||| (==','))) . split null . lines $ input
    let incorrectPrints = filter (not . correct rules) prints
    print $ sum . map (middle . reorder rules) $ incorrectPrints