
module DetermineTheType where

example = 1

x = 5

y = x + 5

w = y * 10

z y = y * 10

--functionH :: [a] -> a
functionH (x:_) = x

--funtionC :: Ord a, Ord b => a -> b -> Bool
functionC x y =
    if (x > y) then True else False

i :: a -> a
i = id

c :: a -> b -> a
c a _ = a

c' :: b -> a -> b
c' a _ = a

r :: [a] -> [a]
r a = fmap id a

co :: (b -> c) -> (a -> b) -> a -> c
co bc ab a =
    bc (ab a) 

a :: (a -> c) -> a -> a
a _ ugh =
    ugh

a' :: (a -> b) -> a -> b
a' f = f