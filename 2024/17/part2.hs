import Data.Bits (Bits(xor))
import Data.Char (isDigit)
import Data.Foldable (find)
import Data.Maybe (fromJust, isJust, fromMaybe)

split :: Char -> String -> [String]
split p str =
    let (x, xs) = break (==p) str
    in if null xs then [x] else x : split p (tail xs)

int :: String -> Int
int = read . filter isDigit

parse :: String -> (Int, [Int])
parse input =
    let (a : _ : _ : _ : text : _) = lines input
    in (int a, map int . split ',' $ text)

{-
* Decompilation of input program

assembly:
    00: bst A
    02: bxl $2
    04: cdv B
    06: bxl $7
    08: bxc
    10: adv $3
    12: out B
    14: jnz $0

decompiled C-like code:
    b = a & 0b111;
    b ^= 0b10;
    c = a >> b;
    b ^= 0b111;
    b ^= c
    a >>= 3;
    print(b & 0b111);
    if (a) goto 0;

* Reverse engineering
note:
- output word is calculated from a only (no state is kept in b or c)
- a is shifted by 3 bits to the right to get next word

calculation of a to output word:
    (((a & 0b111) ^ 0b101) ^ (a >> ((a & 0b111) ^ 0b010))) & 0b111
<=> a{~3,2,~1} ^ a{3+a{3,~2,1},2+a{3,~2,1},1+a{3,~2,1}}
<=> ~a{3} ^ a{3+a{3,~2,1}} , a{2} ^ a{2+a{3,~2,1}} , ~a{1} ^ a{2+a{3,~2,1}}

* We want to print 2,4,1,2,7,5,1,7,4,4,0,3,5,5,3,0
-}

algorithm :: Int -> Int
algorithm a =
    let a3 = a `mod` 8 in
    (a3 `xor` 0b101 `xor` a `div` 2 ^ (a3 `xor` 0b010)) `mod` 8

generate :: Int -> [Int] -> Maybe Int
generate a (w : ws) =
    fromMaybe Nothing . find isJust . map f $ [a..a + 7]
    where
        f a
            | w /= algorithm a = Nothing
            | null ws = Just a
            | otherwise = generate (a * 8) ws

test :: Int -> [Int]
test 0 = []
test a = a : test (a `div` 8)

main :: IO ()
main = do
    input <- readFile "2024/17/input.txt"
    let (a, code) = parse input
    print $ map algorithm (test a) -- should equal the solution of part 1
    let solution = fromJust . generate 0 . reverse $ code
    print $ map algorithm (test solution) -- should equal the code
    print code
    print solution