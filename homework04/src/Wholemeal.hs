----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 04
--
----------------------------------------------------------------------

module Wholemeal where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3*n + 1)

-- |
--
-- >>> fun1 [1,3,5,7] == fun1' [1,3,5,7]
-- True
-- >>> fun1 [1,2,3] /= fun1' [1,2,3]
-- False
-- >>> fun2 10 == fun2' 10
-- True
-- >>> fun2 15 /= fun2' 15
-- False

fun1' :: [Integer] -> Integer
fun1' = foldl (\acc x -> if even x then (x-2)*acc else acc ) 1

fun2' :: Integer -> Integer
fun2' n = foldl (\acc x -> if even x then x+acc else acc) 0 $ takeWhile (>1) $ iterate (\x -> if even x then x `div` 2 else 3*x + 1 ) n


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

data Tree a =
    Leaf
  | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: Eq a => [a] -> Tree a
foldTree xs = foldr (insertChild start) Leaf xs
  where start = floor (logBase 2 $ fromIntegral(length xs)::Double)

insertChild :: Int -> a -> Tree a -> Tree a
insertChild _ _ _ = Leaf
-- insertChild _ x (Node n left y right)
--           | right == Leaf = Node n left y (Node (n-1) Leaf x Leaf)
--           | otherwise = Node n (Node (n-1) Leaf x Leaf) y right
-- insertChild start x _ = Node start Leaf x Leaf

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> xor [False, True, False]
-- True
-- >>> xor [False, True, False, False, True]
-- False

xor :: [Bool] -> Bool
xor = foldr xor2 False
  where xor2 x y = (x || y) && not (x && y)

-- |
--
-- >>> map' (+1) [1,2,3]
-- [2,3,4]

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Optional

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f x = foldr (flip f) x . reverse

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

sieveSundaram :: Integer -> [Integer]
sieveSundaram = undefined
-- sieveSundaram n = map ((+1) . (*2)) $ [1..n] \\ sudaram
--   where sudaram = map (\(i, j) -> i + j + 2*i*j)
--                 . filter (\(i, j) -> i + j + 2*i*j <= n) $ cartProd [1..n] [1..n]

-- -- Return all possible pairs
-- cartProd :: [a] -> [b] -> [(a, b)]
-- cartProd xs ys = [(x,y) | x <- xs, y <- ys]
