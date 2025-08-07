import Data.Char (isDigit)

split :: Char -> String -> [String]
split p str =
    let (x, xs) = break (==p) str
    in if null xs then [x] else x : split p (tail xs)

int :: String -> Int
int = read . filter isDigit

str :: Int -> String
str = show

literal :: Int -> String
literal = ('$':) . str

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (a : b : xs) = (a, b) : pairs xs

adv :: Int
adv = 0
bxl :: Int
bxl = 1
bst :: Int
bst = 2
jnz :: Int
jnz = 3
bxc :: Int
bxc = 4
out :: Int
out = 5
bdv :: Int
bdv = 6
cdv :: Int
cdv = 7

combo :: Int -> String
combo x
    | 0 <= x && x <= 3 = literal x
    | x == 4 = "A"
    | x == 5 = "B"
    | x == 6 = "C"
    | x == 7 = error "invalid combo operand 7"

disassemble :: Int -> Int -> [String]
disassemble opcode operand
    | opcode == adv = ["adv", combo operand]
    | opcode == bxl = ["bxl", literal operand]
    | opcode == bst = ["bst", combo operand]
    | opcode == jnz = ["jnz", literal operand]
    | opcode == bxc = ["bxc"]
    | opcode == out = ["out", combo operand]
    | opcode == bdv = ["bdv", combo operand]
    | opcode == cdv = ["cdv", combo operand]

label :: [[String]] -> [[String]]
label = zipWith (:) (map (tail . (++":") . str) [100,102..])

dec :: Int -> String
dec = str

bin :: Int -> String
bin 0 = "0b0"
bin x = '0' : 'b' : reverse (bin x)
    where
        bin 0 = ""
        bin x = (if even x then '0' else '1') : bin (x `div` 2)

comboDec :: Int -> String
comboDec x
    | 0 <= x && x <= 3 = dec x
    | x == 4 = "a"
    | x == 5 = "b"
    | x == 6 = "c"
    | x == 7 = error "invalid combo operand 7"

comboBin :: Int -> String
comboBin x
    | 0 <= x && x <= 3 = bin x
    | x == 4 = "a & 0b111"
    | x == 5 = "b & 0b111"
    | x == 6 = "c & 0b111"
    | x == 7 = error "invalid combo operand 7"

decompile :: Int -> Int -> String
decompile opcode operand
    | opcode == adv = "a >>= " ++ comboDec operand ++ ";"
    | opcode == bxl = "b ^= " ++ bin operand ++ ";"
    | opcode == bst = "b = " ++ comboBin operand ++ ";"
    | opcode == jnz = "if (a) goto " ++ dec operand ++ ";"
    | opcode == bxc = "b ^= c"
    | opcode == out = "print(" ++ comboBin operand ++ ");"
    | opcode == bdv = "b = a >> " ++ comboDec operand ++ ";"
    | opcode == cdv = "c = a >> " ++ comboDec operand ++ ";"

parse :: String -> [Int]
parse input =
    let (_ : _ : _ : _ : text : _) = lines input
    in map int . split ',' $ text

main :: IO ()
main = do
    input <- readFile "2024/17/input.txt"
    putStrLn "assembly:"
    putStrLn $ unlines . map (("  "++) . unwords) . label . map (uncurry disassemble ). pairs . parse $ input
    putStrLn "decompiled C-like code:"
    putStrLn $ unlines . map (("  "++) . uncurry decompile) . pairs . parse $ input