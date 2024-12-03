import Text.Regex.TDFA ((=~), AllMatches(getAllMatches), AllTextMatches (getAllTextMatches))
import Data.Maybe (fromMaybe)
import Data.List (find)

int :: String -> Int
int = read

doOperation :: String
doOperation = "do\\(\\)"

don'tOperation :: String
don'tOperation = "don't\\(\\)"

mulOperation :: String
mulOperation = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

opIndecies :: String -> String -> [Int]
opIndecies op x = map fst (getAllMatches (x =~ op) :: [(Int, Int)])

doIndecies :: String -> [Int]
doIndecies = opIndecies doOperation

don'tIndecies :: String -> [Int]
don'tIndecies = opIndecies don'tOperation

mulIndecies :: String -> [Int]
mulIndecies = opIndecies mulOperation

extractOperations :: String -> [String]
extractOperations x = getAllTextMatches (x =~ mulOperation)

isEnabled :: [Int] -> [Int] -> Int -> Bool
isEnabled ren rdis x = fromMaybe 0 (find (<x) ren) >= fromMaybe 0 (find (<x) rdis)

maskEnableds :: [Int] -> [Int] -> [Int] -> [Bool]
maskEnableds en dis = map (isEnabled (reverse en) (reverse dis))

filterByMask :: [Bool] -> [b] -> [b]
filterByMask mask xs = map snd . filter fst $ zip mask xs

fth :: (a, b, c, d) -> d
fth (_, _, _, x) = x

getOperands :: String -> [Int]
getOperands x = map int $ fth (x =~ mulOperation :: (String, String, String, [String]))

main :: IO ()
main = do
    input <- readFile "2024/03/input.txt"
    let mask = maskEnableds (doIndecies input) (don'tIndecies input) (mulIndecies input)
    print $ sum . map (product . getOperands) . filterByMask mask . extractOperations $ input