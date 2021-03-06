----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 03
--
----------------------------------------------------------------------

module Golf where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> skips "ABCD"
-- ["ABCD","BD","C","D"]
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
-- >>> skips [1]
-- [[1]]
-- >>> skips [True, False]
-- [[True,False],[False]]
-- >>> skips []
-- []

f ::[a] -> Int-> [a]
f   []   _ = []
f (x:xs) n = x: (f $ drop m xs) n
    where m = n - 1

skips :: [a] -> [[a]]
skips = skips' 0


skips' ::Int -> [a] -> [[a]]
skips' _ [] = []
skips' n l@(_:xs) =  f l m : skips' m xs
    where m =n+1

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
-- >>> localMaxima [2,3,4,1,5]
-- [4]
-- >>> localMaxima [1,2,3,4,5]
-- []

localMaxima :: [Integer] -> [Integer]
localMaxima []=        []
localMaxima [_]        = []
localMaxima [_,_]=     []
localMaxima (x:y:z:xs)= a ++ localMaxima (y:z:xs)
    where a= [y | y > x && y > z]

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> histogram [1,1,1,5]
-- " *        \n *        \n *   *    \n==========\n0123456789\n"
import  Data.fromMaybe
import  Data.List

histogram :: [Integer] -> String
histogram n = ss
    where
        fLookupInv = flip lookup
        fListarRepeticiones = map (\l@(x:_) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
        fNumRepeticiones = fromMaybe 0 . fLookupInv fListarRepeticiones
        serie = [0..9]
        num = map fNumRepeticiones serie -- [0,1,2,3,4,5,6,7,8,9]  --[0,4,7,2,0,1,0,0,0]
        arr = map (`replicate` "*") num
        lis = map concat arr
        los= transpose.map (reverse.fullfill 10 ' ') $lis
        ig= concat.replicate 10 $"="
        fin = los ++ [ig] ++ [concat.map show $serie]
        ss= intercalate "\n" fin

fullfill ::Int->Char -> String-> String
fullfill n c s
        | n<0 = s
        | otherwise =  s ++ aux
        where
            len = length s
            m = if n-len > 0 then n-len else 0
            aux= replicate m c

