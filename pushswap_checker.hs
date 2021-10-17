--
-- EPITECH PROJECT, 2021
-- B-FUN-300-STG-3-1-pushswapchecker-jonathan.cohen
-- File description:
-- pushswap_checker
--

import System.Environment
import System.Exit
import Data.Char

push :: ([Int], [Int]) -> [Char] -> ([Int], [Int])
push (l_a, l_b) (x : xs)
    | 'a' <- x = pa l_a l_b
    | 'b' <- x = pb l_a l_b
    | otherwise = error "Invalid option of push"

rotateB :: ([Int], [Int]) -> [Char] -> ([Int], [Int])
rotateB (l_a, l_b) [] = rr l_a l_b
rotateB (l_a, l_b) (x : xs)
    | ' ' <- x = rr l_a l_b
    | 'a' <- x = ((rra l_a), l_b)
    | 'b' <- x = (l_a, (rrb l_b))
    | 'r' <- x = rrr l_a l_b
    | otherwise = error "Invalid option of rotate"

rotate :: ([Int], [Int]) -> [Char] -> ([Int], [Int])
rotate (l_a, l_b) (x : xs)
    | 'a' <- x = ((ra l_a), l_b)
    | 'b' <- x = (l_a, (rb l_b))
    | 'r' <- x = rotateB (l_a, l_b) xs
    | otherwise = error "Invalid option of rotate"

swap :: ([Int], [Int]) -> [Char] -> ([Int], [Int])
swap (l_a, l_b) (x : xs)
    | 'a' <- x = ((sa l_a), l_b)
    | 'b' <- x = (l_a, (sb l_b))
    | 'c' <- x = sc l_a l_b
    | otherwise = error "Invalid option of swap"

exec :: ([Int], [Int]) -> [Char] -> ([Int], [Int])
exec ll (x:xs)
    | 's' <- x = swap ll xs
    | 'p' <- x = push ll xs
    | 'r' <- x = rotate ll xs
    | otherwise = error "invalid character"

nextSpace :: [Char] -> [Char]
nextSpace [] = []
nextSpace (x:xs)
    | ' ' <- x = xs
    | otherwise = nextSpace (xs)

parsing :: ([Int], [Int]) -> [Char] -> ([Int], [Int])
parsing (l_a, l_b) [] = (l_a, l_b)
parsing (l_a, l_b) cmds = parsing (exec (l_a, l_b) cmds) (nextSpace cmds)

toInt :: [String] -> [Int]
toInt [] = []
toInt (x:xs) =
    case (reads x) :: [(Int, String)] of
        [(value, "")] -> value : toInt xs
        _ -> error "Not a interger"

checkIfOrder :: [Int] -> Int -> Bool
checkIfOrder [] _ = True
checkIfOrder (x:xs) y
    | x >= y = checkIfOrder xs x
    | otherwise = False

check :: ([Int], [Int]) -> Bool
check (a, b)
    | length b > 0 = False
check ([], _) = False
check ((x:xs), _) = checkIfOrder xs x

main :: IO ()
main = do
    args <- getArgs
    line <- getLine
    let list = (toInt args)
    let parse = parsing (list, []) line
    let display | (check parse) = putStrLn "OK"
                | otherwise = putStr "KO: " >> print parse
    display

-- NOTE - swap the first two elements of l_a
sa :: [a] -> [a]
sa [] = []
sa l_a
    | (length l_a) < 2 = l_a
sa (x:y:xs) = (y:x:xs)

sb :: [a] -> [a]
sb l_b = sa l_b

-- NOTE -- Process the two swap
sc :: [a] -> [a] -> ([a], [a])
sc l_a l_b = (sa l_a, sb l_b)

pa :: [a] -> [a] -> ([a], [a])
pa a [] = (a, [])
pa a (y: ys) = ((y : a), ys)

pb :: [a] -> [a] -> ([a], [a])
pb [] b = ([], b)
pb (x:xs) b = (xs, x : b)

ro :: [a] -> a -> [a]
ro [] a = [a]
ro (x:xs) a = x : (ro xs a)

ra :: [a] -> [a]
ra [] = []
ra (x:xs) = (ro xs x)

rb :: [a] -> [a]
rb [] = []
rb (x:xs) = (ro xs x)

rr :: [a] -> [a] -> ([a], [a])
rr a b = (ra a, rb b)

rra :: [a] -> [a]
rra [] = []
rra a = (myLast a) : (myInit a)

rrb :: [b] -> [b]
rrb [] = []
rrb b = (myLast b) : (myInit b)

rrr :: [a] -> [a] -> ([a], [a])
rrr a b = (rra a, rrb b)

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast (x : xs)
    | (length xs) == 0 = x
    | otherwise = myLast xs

myInit :: [a] -> [a]
myInit [] = error "no list"
myInit (x : xs)
    | (length xs) == 0 = []
    | otherwise = x : (myInit xs)
