import GHC.Arr (Array, (!), bounds, listArray)
import Data.Bits (Bits(xor))
import Data.Foldable (find)
import Data.Char (isDigit)

-- LIB:
split :: Char -> String -> [String]
split p str =
    let (x, xs) = break (==p) str
    in if null xs then [x] else x : split p (tail xs)

unsplit :: Char -> [String] -> String
unsplit _ [str] = str
unsplit c (str : strs) = str ++ c : unsplit c strs

int :: String -> Int
int = read . filter isDigit

str :: Int -> String
str = show

-- MACHINE:
data MachinePart = IP | A | B | C | OUT deriving (Eq, Show)
type MachineState = (Int, Int, Int, Int, [Int])
type Memory = MachinePart -> (MachineState, MachinePart)
type Text = Array Int Int
type Machine = (Memory, Text)
type Instruction = Memory -> Int -> Memory

createMemory :: MachineState -> Memory
createMemory = curry id

infix 9 ?
(?) :: Memory -> MachinePart -> Int
(?) memory IP = let ((ip, _, _, _, _), _) = memory IP in ip
(?) memory A = let ((_, a, _, _, _), _) = memory A in a
(?) memory B = let ((_, _, b, _, _), _) = memory B in b
(?) memory C = let ((_, _, _, c, _), _) = memory C in c
(?) _ OUT = error "can't read from out stream"

infix 1 $=
($=) :: (MachineState, MachinePart) -> Int -> Memory
($=) ((ip, a, b, c, out), IP) ip' = createMemory (ip', a, b, c, out)
($=) ((ip, a, b, c, out), A) a' = createMemory (ip, a', b, c, out)
($=) ((ip, a, b, c, out), B) b' = createMemory (ip, a, b', c, out)
($=) ((ip, a, b, c, out), C) c' = createMemory (ip, a, b, c', out)
($=) ((ip, a, b, c, out), OUT) out' = createMemory (ip, a, b, c, out' : out)

outStream :: Memory -> [Int]
outStream memory = let ((_, _, _, _, out), _) = memory OUT in reverse out

infix 9 @
(@) :: Int -> Memory -> Int
(@) combo memory
    | 0 <= combo && combo <= 3 = combo
    | combo == 4 = memory ? A
    | combo == 5 = memory ? B
    | combo == 6 = memory ? C
    | combo == 7 = error "invalid combo operand 7"

adv :: Instruction
adv memory x = memory A $= memory ? A `div` 2 ^ x @ memory

bxl :: Instruction
bxl memory x = memory B $= memory ? B `xor` x

bst :: Instruction
bst memory x = memory B $= x @ memory `mod` 8

jnz :: Instruction
jnz memory x
    | memory ? A == 0 = memory
    | otherwise = memory IP $= x - 2

bxc :: Instruction
bxc memory _ = memory B $= memory ? B `xor` memory ? C

out :: Instruction
out memory x = memory OUT $= x @ memory `mod` 8

bdv :: Instruction
bdv memory x = memory B $= memory ? A `div` 2 ^ x @ memory

cdv :: Instruction
cdv memory x = memory C $= memory ? A `div` 2 ^ x @ memory

opcode :: Int -> Instruction
opcode x
    | x == 0 = adv
    | x == 1 = bxl
    | x == 2 = bst
    | x == 3 = jnz
    | x == 4 = bxc
    | x == 5 = out
    | x == 6 = bdv
    | x == 7 = cdv

halts :: Machine -> Bool
halts (memory, text) = memory ? IP + 1 > snd (bounds text)

step :: Machine -> Machine
step (memory, text)
    | halts (memory, text) = error "machine halts"
    | otherwise =
        let ip = memory ? IP
            memory' = opcode (text ! ip) memory (text ! (ip + 1))
        in (memory' IP $= memory' ? IP + 2, text)

eval :: Machine -> [Int]
eval machine
    | halts machine = outStream (fst machine)
    | otherwise = eval (step machine)

buildMachine :: (Int, Int, Int, [Int]) -> Machine
buildMachine (a, b, c, texts) = (createMemory (0, a, b, c, []), listArray (0, length texts - 1) texts)

-- PARSING:
parse :: String -> (Int, Int, Int, [Int])
parse input =
    let (a : b : c : _ : text : _) = lines input
    in (int a, int b, int c, map int . split ',' $ text)

render :: [Int] -> String
render = unsplit ',' . map str

main :: IO ()
main = do
    input <- readFile "2024/17/input.txt"
    putStrLn $ render . eval . buildMachine . parse $ input