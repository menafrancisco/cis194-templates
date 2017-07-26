----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 01
--
----------------------------------------------------------------------

module Basis where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> toDigits 1234
-- [1,2,3,4]
-- >>> toDigits 0
-- []
-- >>> toDigits (-17)
-- []

toDigits :: Integer -> [Integer]
--  toDigits = undefined
toDigits n
    | n <= 0 = []
    | otherwise = map convertCharToInteger(show n)
    where convertCharToInteger c = read[c]::Integer




----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]

doubleEveryOther :: [Integer] -> [Integer]
--doubleEveryOther = undefined
doubleEveryOther [] = []
doubleEveryOther n = reverse (f n)
    where
        serie= 1:2:serie
        f = zipWith (*) serie.reverse





----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> sumDigits [16,7,12,5]
-- 22

sumDigits :: [Integer] -> Integer
sumDigits = undefined

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- |
--
-- >>> validate 4012888888881881
-- True
-- >>> validate 4012888888881882
-- False

validate :: Integer -> Bool
validate = undefined

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

type Peg = String
type Move = (Peg, Peg)

-- |
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' = undefined
